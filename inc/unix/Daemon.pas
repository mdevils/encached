unit Daemon;
{ Network daemon. Manages TServerThreadManagers. }

{$mode objfpc}{$H+}
{$define usecthreads}

interface

uses
	{$IFDEF UNIX}{$IFDEF UseCThreads} cthreads, {$ENDIF}{$ENDIF}
	Sysutils, Classes, DaemonApp, ServerThreadManager, ServerThread;

type

	{ Network daemon }
	TUnixDaemon = class(TCustomDaemon)
	public
		{ Initializer }
		function Start: boolean; override;
		{ Execute actions }
		function Execute: boolean; override;
		{ Deinitializer }
		function ShutDown: boolean; override;
		{ Actions on install }
		function Install: boolean; override;
		{ Actions on uninstall }
		function UnInstall: boolean; override;
	end;
	
	{ Daemon mapper. Register/unregister daemon. }
	TUnixDaemonMapper = class(TCustomDaemonMapper)
		constructor Create(AOwner: TComponent); override;
	end;
	
	{ Listener registration record }
	TServerThreadRecord = record
		{ Port number 0-65535 }
		Port: longint;
		{ Network thread class }
		ThreadClass: TServerThreadClass;
		{ Thread manager instance }
		Thread: TServerThreadManager;
	end;
	
	{ Registers server thread }
	procedure RegisterServerThread(Port: longint; ThreadClass: TServerThreadClass);

	var ServerThreads: array of TServerThreadRecord;

implementation

	procedure RegisterServerThread(Port: longint; ThreadClass: TServerThreadClass);
	var l: longint; rec: TServerThreadRecord;
	begin
		l := Length(ServerThreads);
		SetLength(ServerThreads, l + 1);
		rec.Port := Port;
		rec.ThreadClass := ThreadClass;
		ServerThreads[l] := rec;
	end; 
	
	{ TUnixDaemon }

	function TUnixDaemon.Start: boolean;
	var l, i: longint;
	begin
		Result := inherited Start;
		l := Length(ServerThreads);
		for i := 0 to l - 1 do
		begin
			ServerThreads[i].Thread := TServerThreadManager.Create(false, ServerThreads[i].Port, ServerThreads[i].ThreadClass);
		end;
	end;

	function TUnixDaemon.Execute: boolean;
	begin
		Result := inherited Execute;
	end;

	function TUnixDaemon.ShutDown: boolean;
	var l, i: longint; 
	begin
		l := Length(ServerThreads);
		try
			for i := 0 to l - 1 do ServerThreads[i].Thread.Quit;
		except
			on E: Exception do WriteLn('Error stopping server thread: ' + E.Message);
		end;
		Result := inherited ShutDown;
	end;

	function TUnixDaemon.Install: boolean;
	begin
		Result := inherited Install;
	end;

	function TUnixDaemon.UnInstall: boolean;
	begin
		Result := inherited UnInstall;
	end;

	{ TUnixDaemonMapper }

	constructor TUnixDaemonMapper.Create(AOwner: TComponent);
	var
		D: TDaemonDef;
	begin
		inherited Create(AOwner);
		D := DaemonDefs.Add as TDaemonDef;
		D.DisplayName := 'Cacher Daemon';
		D.Name := 'CacherDaemon';
		D.DaemonClassName := 'TUnixDaemon';
	end;

begin
	
end.
