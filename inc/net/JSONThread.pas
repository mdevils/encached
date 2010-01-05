unit JSONThread;

interface

uses
	SysUtils, Classes, ServerThread, JSONParser, fpJSON;

type
	TJSONThread = class(TServerThread)
	protected
		StringBuffer: AnsiString;
		procedure DataRecieved; override;
		procedure ExecJSON;
		procedure ProcessJSON(Data: TJSONData); virtual; abstract;
		procedure SendJSON(Data: TJSONData);
	end;

implementation 

	procedure TJSONThread.SendJSON(Data: TJSONData);
	var s: AnsiString;
	begin
		s := Data.AsJSON;
		Socket.WriteStr(IntToStr(Length(s)) + #13#10 + s);
	end; 

	procedure TJSONThread.ExecJSON;
	var bp, l, jl: longint;
		len: ShortString;
		json: AnsiString;
		jsonData: TJSONData;
		jsonParser: TJSONParser;
	begin
		l := Length(StringBuffer);
		if l = 0 then exit;
		bp := 1;
		while (bp <= l) and (StringBuffer[bp] in [#13, #10]) do Inc(bp);
		if bp < l then
		begin
			len := '';
			while (bp <= l) and (StringBuffer[bp] <> #13) do
			begin
				len := len + StringBuffer[bp];
				Inc(bp);
			end;
			if (Length(len) > 0) and (bp < l) then
			begin
				Inc(bp);
				if StringBuffer[bp] = #10 then
				begin
					Inc(bp);
					try
						jl := StrToInt(len);
					except
						WriteLn('Invalid package length: ' + len);
						QuitNow := true;
						exit;
					end;
					if (bp + jl) <= (l + 1) then
					begin
						json := Copy(StringBuffer, bp, jl);
						try
							jsonParser := TJSONParser.Create(json);
							jsonData := jsonParser.Parse;
							WriteLn(jsonData.AsJSON);
						except
							WriteLn('JSON Parse error');
						end;
						bp := bp + jl;
						if bp > l then StringBuffer := ''
						else StringBuffer := Copy(StringBuffer, bp, l - bp);
					end;
				end;
			end;
		end
		else
		begin
			if bp <> 1 then
			begin
				if bp > l then StringBuffer := ''
				else StringBuffer := Copy(StringBuffer, bp, l - bp);
			end;
		end;
	end;

	procedure TJSONThread.DataRecieved;
	var s: AnsiString;
	begin
		s := Socket.ReadStr;
		if s <> '' then
		begin
			StringBuffer := StringBuffer + s;
			ExecJSON;
		end;
	end;

end.
