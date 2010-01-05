unit CacherThread;

interface

uses
	SysUtils, Classes, ServerThread, StrUtils, Hashtable;

const
	{ Parsing constants }
	TOKEN_UNKNOWN = 0;
	TOKEN_STRING  = 1;
	TOKEN_END     = 2;

type
	{ Parsing token }
	TCacherThreadToken = record
		{ TOKEN_XXX }
		Token: byte;
		{ String value }
		Value: AnsiString;
		{ Starts at }
		Start: longint;
		{ Ends at }
		Finish: longint;
	end;
	
	{ Cache thread }
	TCacherThread = class(TServerThread)
	protected
		{ String buffer }
		Buffer: AnsiString;
		{ String buffer length }
		BufferLength: longint;
		{ CurrentPos in the buffer }
		CurrentPos: longint;
		{ Launched when network data recieved }
		procedure DataRecieved; override;
		{ Processes imcoming requests }
		procedure ProcessData;
		{ Returns next token }
		function GetToken(p: longint): TCacherThreadToken;
		{ Returns next string }
		function GetString: AnsiString;
		{ Returns next uint }
		function GetUInteger: longint;
		{ Skips space characters }
		procedure SkipSpaces;
	end;

var
	{ Cache hashtable }
	CacheHashtable: THashtable;
	
implementation 

	procedure TCacherThread.SkipSpaces;
	begin
		while (CurrentPos <= BufferLength) and (Buffer[CurrentPos] in [#10, #13, ' ', #9]) do Inc(CurrentPos);
	end; 

	function TCacherThread.GetUInteger: longint;
	var Token: TCacherThreadToken; code: integer;
	begin
		Token := GetToken(CurrentPos);
		if Token.Token = TOKEN_STRING then
		begin
			Val(Token.Value, result, code);
			if code <> 0 then result := -1;
		end else result := -1;
		CurrentPos := Token.Finish + 1;
	end; 

	function TCacherThread.GetString: AnsiString;
	var Token: TCacherThreadToken;
	begin
		result := '';
		Token := GetToken(CurrentPos);
		if Token.Token = TOKEN_STRING then result := Token.Value;
		CurrentPos := Token.Finish + 1;
	end; 

	function TCacherThread.GetToken(p: longint): TCacherThreadToken;
	begin
		while (p <= BufferLength) and (Buffer[p] in [#10, #13, ' ', #9]) do Inc(p);
		if p > BufferLength then
		begin
			result.Token := TOKEN_END;
			result.Finish := p;
			result.Start := p;
		end;
		result.Start := p;
		result.Token := TOKEN_STRING;
		while (p <= BufferLength) and not (Buffer[p] in [#10, #13, ' ', #9]) do Inc(p);
		result.Value := Copy(Buffer, result.Start, p - result.Start);
		result.Finish := p - 1;
	end;

	procedure TCacherThread.ProcessData;
	var L: longint; Cmd, Key: AnsiString; P: Pointer; Data, CompareData: PAnsiString;
	begin
		while true do
		begin
			CurrentPos := 1;
			SkipSpaces;
			if PosEx(#13#10, Buffer, CurrentPos) <> 0 then
			begin
				Cmd := GetString;
				if Cmd = 'GET' then
				begin
					L := GetUInteger;
					if L = -1 then
					begin
						QuitNow := true;
						exit;
					end;
					Inc(CurrentPos, 1);
					if CurrentPos > BufferLength then
					begin
						QuitNow := true;
						exit;
					end;
					Key := Copy(Buffer, CurrentPos, L);
					Inc(CurrentPos, L);
					SkipSpaces;
					Buffer := Copy(Buffer, CurrentPos, BufferLength - CurrentPos);
					BufferLength := Length(Buffer);
					
					WriteLn('GET ' + Key);
					P := CacheHashtable.Get(Key);
					if P = nil then
						Socket.WriteStr('NODATA' + #13#10)
					else
						Socket.WriteStr('DATA ' + IntToStr(Length(PAnsiString(P)^)) + ' ' + PAnsiString(P)^ + #13#10);
				end
				else
				if Cmd = 'PUT' then
				begin
					L := GetUInteger;
					if L = -1 then
					begin
						QuitNow := true;
						exit;
					end;
					Inc(CurrentPos, 1);
					if CurrentPos > BufferLength then
					begin
						QuitNow := true;
						exit;
					end;
					Key := Copy(Buffer, CurrentPos, L);
					Inc(CurrentPos, L);
					L := GetUInteger;
					if L = -1 then
					begin
						QuitNow := true;
						exit;
					end;
					while (CurrentPos <= BufferLength) and (Buffer[CurrentPos] <> #13) do Inc(CurrentPos);
					Inc(CurrentPos, 2);
					if CurrentPos + L > BufferLength + 1 then
					begin
						exit;
					end;
					New(Data);
					Data^ := Copy(Buffer, CurrentPos, L);
					Inc(CurrentPos, L);
					SkipSpaces;
					Buffer := Copy(Buffer, CurrentPos, BufferLength - CurrentPos);
					BufferLength := Length(Buffer);
					WriteLn('SET ' + Key + '=' + Data^);
					try
						CompareData := PAnsiString(CacheHashtable.Get(Key));
						if (CompareData <> nil) and (CompareData^ = Data^) then
						begin
							Socket.WriteStr('HIT' + #13#10)
						end
						else
						begin
							CacheHashtable.Put(Key, Data);
							Socket.WriteStr('SUCCESS' + #13#10);
						end;
					except
						Socket.WriteStr('FAILURE' + #13#10);
					end;
				end
				else
				if Cmd = 'REMOVE' then
				begin
					L := GetUInteger;
					if L = -1 then
					begin
						QuitNow := true;
						exit;
					end;
					Inc(CurrentPos, 1);
					if CurrentPos > BufferLength then
					begin
						QuitNow := true;
						exit;
					end;
					Key := Copy(Buffer, CurrentPos, L);
					Inc(CurrentPos, L);
					SkipSpaces;
					Buffer := Copy(Buffer, CurrentPos, BufferLength - CurrentPos);
					BufferLength := Length(Buffer);

					WriteLn('GET ' + Key);
					P := CacheHashtable.Remove(Key);
					if P = nil then
						Socket.WriteStr('FAILURE' + #13#10)
					else
					begin
						Data := PAnsiString(P);
						Dispose(Data);
						Socket.WriteStr('SUCCESS' + #13#10);
					end;
				end
				else
				begin
					QuitNow := true;
					exit;
				end;
			end
			else
			begin
				exit;
			end;
		end;
	end; 

	procedure TCacherThread.DataRecieved;
	begin
		Buffer := Buffer + Socket.ReadStr;
		BufferLength := Length(Buffer);
		ProcessData;
	end;

	procedure ClearHashtable(Key: AnsiString; Value: Pointer);
	var PS: PAnsiString;
	begin
		PS := PAnsiString(Value);
		Dispose(PS);
	end;

initialization
	CacheHashtable := THashtable.Create;

finalization
	CacheHashtable.Clear(@ClearHashtable);
	CacheHashtable.Destroy;
end.