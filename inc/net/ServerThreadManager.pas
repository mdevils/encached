unit ServerThreadManager;

interface

uses MessagingThread, ServerThread, SysUtils, Classes, NetworkSocket;

type
	{ Listens for connections and creates new thread for each }
	TServerThreadManager = class(TMessagingThread)
	public
		{ Constructor }
		constructor Create(CreateSuspended: boolean; Port: longint; ThreadClass: TServerThreadClass);
	protected
		{ Listening socket }
		Socket: TNetworkSocket;
		{ Thread pool }
		ServerThreads: TList;
		{ Threads class }
		ServerThreadClass: TServerThreadClass;
		{ Server port }
		ServerPort: longint;
		{ Handles thread messages }
		procedure HandleMessage(Message: PMsg); override;
		{ Initializes thread }
		procedure Init; override;
		{ Deinitializes thread }
		procedure Deinit; override;
		{ Custom thread message loop }
		procedure Loop; override;
	end;

implementation 

	constructor TServerThreadManager.Create(CreateSuspended: boolean; Port: longint; ThreadClass: TServerThreadClass);
	begin
		ServerThreadClass := ThreadClass;
		ServerPort := Port;
		ServerThreads := TList.Create;
		inherited Create(CreateSuspended);
	end;

	procedure TServerThreadManager.Loop;
	begin
		try
			if Socket.HasConnections(10) then
			begin
				ServerThreads.Add(ServerThreadClass.Create(Socket.Accept));
			end;
		except
			on E: Exception do
			begin
				WriteLn('Error listening for connections: ' + E.Message);
				QuitNow := true;
			end;
		end;
	end;

	procedure TServerThreadManager.HandleMessage(Message: PMsg);
	begin
	end;

	procedure TServerThreadManager.Init;
	begin
		try
			UseSuspending := false;
			Socket := TNetworkSocket.Create;
			Socket.Bind('0.0.0.0', ServerPort);
			Socket.Listen;
		except
			on E: Exception do WriteLn('Cannot start server: ' + E.Message);
		end;
	end;

	procedure TServerThreadManager.Deinit;
	var i, c: longint;
	begin
		Socket.Close;
		c := ServerThreads.Count - 1;
		for i := 0 to c do
		begin
			try
				TMessagingThread(ServerThreads.Items[i]).Quit;
			except
				on E: Exception do WriteLn('Error stopping thread: ' + E.Message);
			end;
		end;
		ServerThreads.Destroy;
	end;

end.
