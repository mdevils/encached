unit Shell;

interface

uses Classes, Streaming, Process, Unix, crt;

function Sys(command: string): string;

implementation


function Sys(command: string): string;
var
	AProcess: TProcess;
	AStringList: TStringList;
begin
	AProcess := TProcess.Create(nil);
	AStringList := TStringList.Create;
	AProcess.CommandLine := command;
	AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
	AProcess.Execute;
	AStringList.LoadFromStream(AProcess.Output);
	result := AStringList.Text;
	AStringList.Free;
	AProcess.Free;
end;

end.
