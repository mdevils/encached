unit MessagingThread;
{ Multithread communicative base thread }

interface

uses {$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
	Classes, SysUtils, Locker, contnrs;

const
	{ Base multi-threading messages }
	MSG_UNKNOWN = 0;
	MSG_QUIT = 1;
	MSG_PING = 2;
	MSG_PONG = 3;

type
	{ Message }
	TMsg = record
		Id: longint;
		Arg: Pointer;
	end;
	PMsg = ^TMsg;
	
	{ Multithread communicative interface }
	IMessagingThread = interface(IInterface)
		procedure PostMessage(Message: PMsg);
	end;
	
	{ Multithread communicative base thread }
	TMessagingThread = class(TThread, IMessagingThread)
	private
		{ Message queue }
		MessageQueue: TQueue;
		{ Message queue locker }
		Locker: TLocker;
		{ Running flag }
		Running: boolean;
	protected
		{ Suspend after each loop, resume on message recieve }
		UseSuspending: boolean;
		{ Quit flag }
		QuitNow: boolean;
		{ Base system handler }
		procedure HandleMessageSys(Message: PMsg);
		{ Custom message handler }
		procedure HandleMessage(Message: PMsg); virtual; abstract;
		{ Initializes thread }
		procedure Init; virtual; abstract;
		{ Deinitializes thread }
		procedure Deinit; virtual; abstract;
		{ Custom actions for each message loop }
		procedure Loop; virtual;
	public
		{ Constructor }
		constructor Create(CreateSuspended: boolean);
		{ Returns true if thread is still running }
		function IsAlive: boolean;
		{ Executes thread }
		procedure Execute; override;
		{ Posts message into the queue }
		procedure PostMessage(Message: PMsg);
		{ Posts message into the queue }
		procedure PostMessage(Id: longint; Arg: Pointer);
		{ Posts message into the queue }
		procedure PostMessage(Id: longint);
		{ Quits thread. Can be used only from outside. }
		procedure Quit;
		{ Destructor }
		destructor Destroy; override;
		
		{ Interface stuff }
		function QueryInterface(const IID: TGUID; out Obj):HResult; stdcall;
		function _AddRef: Integer; stdcall;
		function _Release: Integer; stdcall;
	end;
	
	PMessagingThread = ^TMessagingThread;

implementation 

	function TMessagingThread.IsAlive: boolean;
	begin
		result := Running;
	end; 

	procedure TMessagingThread.Loop;
	begin
	end; 

	procedure TMessagingThread.Quit;
	begin
		PostMessage(MSG_QUIT);
		while Running do
		begin
			Sleep(1);
		end;
		Terminate;
	end;

	function TMessagingThread.QueryInterface(const IID: TGUID; out Obj):HResult; stdcall;
	begin if GetInterface(IID, Obj) then Result := 0 else Result := HResult($80004002); end;
	function TMessagingThread._AddRef: Integer; stdcall; begin Result := 1; end;
	function TMessagingThread._Release: Integer; stdcall; begin Result := 1; end;

	procedure TMessagingThread.PostMessage(Id: longint);
	begin
		PostMessage(Id, nil);
	end;

	procedure TMessagingThread.PostMessage(Id: longint; Arg: Pointer);
	var M: PMsg;
	begin
		New(M);
		M^.Id := Id;
		M^.Arg := Arg;
		PostMessage(M);
	end;

	constructor TMessagingThread.Create(CreateSuspended: boolean);
	begin
		MessageQueue := TQueue.Create;
		Locker := TLocker.Create;
		QuitNow := false;
		UseSuspending := true;
		inherited Create(CreateSuspended);
	end;
	
	procedure TMessagingThread.HandleMessageSys(Message: PMsg);
	var response: PMsg;
	begin
		case Message^.Id of
			MSG_QUIT:
			begin
				QuitNow := true;
			end;
			MSG_PING:
			begin
				New(response);
				response^.Id := MSG_PONG;
				response^.Arg := @self;
				PMessagingThread(Message^.Arg)^.PostMessage(response);
			end;
		else
			HandleMessage(Message);
		end;
	end;
	
	procedure TMessagingThread.Execute;
	begin
		Init;
		Running := true;
		while true do
		begin
			Locker.Lock;
			try
				while MessageQueue.Count <> 0 do
				begin
					HandleMessageSys(PMsg(MessageQueue.Pop));
					Dispose(PMsg(MessageQueue.Pop));
				end;
			finally
				Locker.Unlock;
			end;
			if QuitNow then
			begin
				DeInit;
				Running := false;
				break;
			end;
			Loop;
			if QuitNow then
			begin
				DeInit;
				Running := false;
				break;
			end;
			if UseSuspending then Suspend;
		end;
	end;

	procedure TMessagingThread.PostMessage(Message: PMsg);	
	begin
		Locker.Lock;
		try
			MessageQueue.Push(Message);
		finally
			Locker.Unlock;
		end;
		if UseSuspending then Resume;
	end;

	destructor TMessagingThread.Destroy;
	begin
		Locker.Destroy;
		inherited Destroy;
	end;

end.