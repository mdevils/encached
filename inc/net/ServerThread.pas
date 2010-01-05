unit ServerThread;

interface

uses MessagingThread, SysUtils, NetworkSocket;

type
	{ Base class for all server threads.
	
	  Supports Sockets, Messaging. }
	TServerThread = class(TMessagingThread)
	public
		{ Constructor }
		constructor Create(Sock: TNetworkSocket);
	protected
		Socket: TNetworkSocket;
		{ Handles custom message }
		procedure HandleMessage(Message: PMsg); override;
		{ Init thread }
		procedure Init; override;
		{ Deinit thread }
		procedure Deinit; override;
		{ Thread message loop }
		procedure Loop; override;
		{ Data recieve event }
		procedure DataRecieved; virtual; abstract;
	end;
	
	{ Some other related class }
	TServerThreadClass = class of TServerThread;
	PServerThread = ^TServerThread;
	
implementation 

	procedure TServerThread.Loop;
	begin
		try
			if Socket.HasData(10) then DataRecieved;
		except
			on E: Exception do
			begin
				QuitNow := true;
			end;
		end;
	end; 

	constructor TServerThread.Create(Sock: TNetworkSocket);
	begin
		Socket := Sock;
		inherited Create(false);
	end; 

	procedure TServerThread.HandleMessage(Message: PMsg);
	begin
		
	end; 

	procedure TServerThread.Init;
	begin
		UseSuspending := false;
	end; 

	procedure TServerThread.Deinit;
	begin
		try
			Socket.Close;
		except
		end;
	end; 

end.
