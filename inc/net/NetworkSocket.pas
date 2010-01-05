unit NetworkSocket;
{ Full async network socket implementation.
  Exceptions raising used for errors. }

interface

uses
	SysUtils, Classes, Sockets, BaseUnix, UnixType;

const
{ Some missing constants }
{$ifdef Darwin}
	MSG_NOSIGNAL = 0;
	SO_NOSIGNAL = $1022;
{$else}
	SO_NOSIGNAL = 0;
{$endif}
	
type
	{ Network Socket buffer}
	TNetworkBuffer = record
		Buffer: Pointer;
		Length: longint;
		MemoryLength: longint;
	end;
	PNetworkBuffer = ^TNetworkBuffer;
	
	{ Simple 1024-byte network data block }
	TNetworkDataBlock1024 = array[0..1023] of char;	
	PNetworkDataBlock1024 = ^TNetworkDataBlock1024;
		
	{ Network Socket using exceptions }
	TNetworkSocket = class
		{ Socket descriptor }
		Sock: cint;
		{ Socket address }
		Address: TInetSockAddr;
		{ Common buffer for string reading }
		CommonBuffer: PNetworkBuffer;
		{ Common data block for string reading }
		CommonDataBlock: PNetworkDataBlock1024;
		{ Constructor }
		constructor Create;
		{ Constructor }
		constructor Create(S: cint; Addr: TInetSockAddr);
		{ Destructor }
		destructor Destroy; override;
		{ Bind server socket to the address/port }
		procedure Bind(Addr: string; Port: integer);
		{ Listen for incoming connections }
		procedure Listen;
		{ Returns connection socket }
		function Accept: TNetworkSocket;
		{ Returns true if incoming connections are available }
		function HasConnections(TimeoutMs: longint): boolean;
		{ Returns true if data is available in buffers }
		function HasData(TimeoutMs: longint): boolean;
		{ Write data to socket }
		procedure Write(Buffer: PNetworkBuffer);
		{ Write string to socket }
		procedure WriteStr(s: AnsiString);
		{ Read data from socket }
		procedure Read(Buffer: PNetworkBuffer);
		{ Read string from socket }
		function ReadStr: AnsiString;
		{ Closes connection }
		procedure Close;
	end;

{ Exception codes for TNetworkSocket operations }
const
	nseIntr = ESysEINTR; // 4: A non blocked signal was caught.
	nseBadf = ESockEBADF; // 9: The socket descriptor is invalid.
	nseAgain = ESysEAGAIN; // 11: Try again.
	nseNoMem = ESysENOMEM; // 12: Select was unable to allocate memory for its internal tables.
	nseAccess = ESockEACCESS; // 13: Address is protected and you don't have permission to open it.
	nseFault = ESysEFAULT; // 14: Bad address.
	nseInval = ESockEINVAL; // 22: The socket is already bound to an address.
	nseNFile = ESysENFILE; // 23: The system file table is full.
	nseMFile = ESockEMFILE; // 24: The per-process descriptor table is full.
	nseNotSock = ESockENOTSOCK; // 38: The descriptor is not a socket.
	nseMsgSize = ESysEMsgSize; // 40: The message cannot be sent atomically.
	nseProtoNoSupport = ESockEPROTONOSUPPORT; // 43: The protocol type or the specified protocol is not supported within this domain.
	nseOpNotSupp = ESysEOPNOTSUPP; // 45: The socket type doesn't support listen operation.
	nseNoBufs = ESockENOBUFS; // 55: Insufficient buffer space is available. The socket cannot be created until sufficient resources are freed.
	nseNotConn = ESockENOTCONN; // 57: The socket isn't connected.
	
	nseConnectionClosed = 1000001; // Connection closed by client

type
	{ Network Socket exception class}
	ENetworkSocketException = class(Exception)
		ErrorCode: longint;
		constructor Create(ErrCode: longint; Msg: string);
	end;

implementation 

	constructor ENetworkSocketException.Create(ErrCode: longint; Msg: string);
	begin
		ErrorCode := ErrCode;
		inherited Create(Msg);
	end;

	procedure TNetworkSocket.Close;
	begin
		FpClose(Sock);		
	end; 

	procedure TNetworkSocket.WriteStr(s: AnsiString);
	var Buffer: PNetworkBuffer;
	begin
		New(Buffer);
		Buffer^.Buffer := pchar(s);
		Buffer^.Length := Length(s);
		try
			Write(Buffer);
		finally
			Dispose(Buffer);
		end;
	end;

	function TNetworkSocket.ReadStr: AnsiString;
	begin
		CommonBuffer^.MemoryLength := sizeof(TNetworkDataBlock1024);
		Read(CommonBuffer);
		if CommonBuffer^.Length > 0 then
			result := Copy(PNetworkDataBlock1024(CommonBuffer^.Buffer)^, 1, CommonBuffer^.Length)
		else
			result := '';
	end; 

	procedure TNetworkSocket.Write(Buffer: PNetworkBuffer);
	var emsg: string; code: longint;
	begin
		if fpSend(Sock, Buffer^.Buffer, Buffer^.Length, MSG_DONTWAIT or MSG_NOSIGNAL) < 0 then
		begin
			code := SocketError;
			case code of
				nseBadf: emsg := 'The socket descriptor is invalid.';
				nseNotSock: emsg := 'The descriptor is not a socket.';
				nseFault: emsg := 'Bad address.';
				nseMsgSize: emsg := 'The message cannot be sent atomically.';
				nseNoBufs: emsg := 'Insufficient buffer space is available. The socket cannot be created until sufficient resources are freed.';
			end;
			emsg := 'Socket write error: ' + emsg;
			raise ENetworkSocketException.Create(code, emsg);			
		end;
	end; 

	procedure TNetworkSocket.Read(Buffer: PNetworkBuffer);
	var emsg: string; code: longint; l: longint;
	begin
		l := fpRecv(Sock, Buffer^.Buffer, Buffer^.MemoryLength, MSG_DONTWAIT or MSG_NOSIGNAL);
		if l < 0 then
		begin
			code := SocketError;
			case code of
				nseBadf: emsg := 'The socket descriptor is invalid.';
				nseNotSock: emsg := 'The descriptor is not a socket.';
				nseFault: emsg := 'Bad address.';
				nseMsgSize: emsg := 'The message cannot be sent atomically.';
				nseNoBufs: emsg := 'Insufficient buffer space is available. The socket cannot be created until sufficient resources are freed.';
				nseNotConn: emsg := 'The socket isn''t connected.';
			end;
			emsg := 'Socket read error: ' + emsg;
			Buffer^.Length := 0;
			raise ENetworkSocketException.Create(code, emsg);
		end
		else
		begin
			Buffer^.Length := l;
		end;
	end;
	
	function TNetworkSocket.Accept: TNetworkSocket;
	var nAddr: TInetSockAddr; nSock: cint; nLen: integer; emsg: string; code: longint;
	begin
		nLen := sizeof(nAddr);
		nAddr := Address;
		nSock := fpAccept(Sock, @nAddr, @nLen);
		if nSock < 0 then
		begin
			code := SocketError;
			case code of
				nseBadf: emsg := 'The socket descriptor is invalid.';
				nseNotSock: emsg := 'The descriptor is not a socket.';
				nseOpNotSupp: emsg := 'The socket type doesn''t support listen operation.';
				nseFault: emsg := 'Bad address.';
			end;
			emsg := 'Socket accept error: ' + emsg;
			raise ENetworkSocketException.Create(code, emsg);
		end else result := TNetworkSocket.Create(nSock, nAddr);
	end; 

	function TNetworkSocket.HasData(TimeoutMs: longint): boolean;
	var timeout: ptimeval; rfds: PFDSet; r: integer; emsg: string; code: longint; arr: array[0..7] of char;
	begin
		New(timeout); timeout^.tv_sec := 0; timeout^.tv_usec := TimeoutMs * 1000;
		New(rfds); fpFD_Zero(rfds^); fpFD_Set(Sock, rfds^);

		r := fpSelect(Sock + 1, rfds, nil, nil, timeout);
		code := SocketError;

		Dispose(timeout); Dispose(rfds);
		
		if r < 0 then
		begin
			case code of
				nseBadf: emsg := 'The socket descriptor is invalid.';
				nseInval: emsg := 'N is negative or too big.';
				nseNoMem: emsg := 'Select was unable to allocate memory for its internal tables.';
				nseIntr: emsg := 'A non blocked signal was caught.';
			end;
			emsg := 'Socket select error: ' + emsg;
			raise ENetworkSocketException.Create(code, emsg);
		end
		else
		if r = 1 then
		begin
			if fpRecv(Sock, @arr, 8, MSG_DONTWAIT or MSG_NOSIGNAL or MSG_PEEK) = 0 then
				raise ENetworkSocketException.Create(nseConnectionClosed, 'Connection closed')
			else result := true;
		end
		else
		begin
			result := false;
		end;
	end;

	function TNetworkSocket.HasConnections(TimeoutMs: longint): boolean;
	var timeout: ptimeval; rfds: PFDSet; r: longint; emsg: string; code: longint;
	begin
		New(timeout); timeout^.tv_sec := 0; timeout^.tv_usec := TimeoutMs * 10000;
		New(rfds); fpFD_Zero(rfds^); fpFD_Set(Sock, rfds^);

		r := fpSelect(Sock + 1, rfds, nil, nil, timeout);
		code := SocketError;

		result := r = 1;

		Dispose(timeout); Dispose(rfds);
		if r < 0 then
		begin
			case code of
				nseBadf: emsg := 'The socket descriptor is invalid.';
				nseInval: emsg := 'N is negative or too big.';
				nseNoMem: emsg := 'Select was unable to allocate memory for its internal tables.';
				nseIntr: emsg := 'A non blocked signal was caught.';
			end;
			emsg := 'Socket select error: ' + emsg;
			raise ENetworkSocketException.Create(code, emsg);
		end;
	end;

	procedure TNetworkSocket.Listen;
	var emsg: string; code: longint;
	begin
		if fpListen(Sock, 0) < 0 then
		begin
			code := SocketError;
			case code of
				nseBadf: emsg := 'The socket descriptor is invalid.';
				nseNotSock: emsg := 'The descriptor is not a socket.';
				nseProtoNoSupport: emsg := 'The socket type doesn''t support the Listen operation.';
			end;
			emsg := 'Socket listen error: ' + emsg;
			raise ENetworkSocketException.Create(code, emsg);
		end;
	end; 

	procedure TNetworkSocket.Bind(Addr: string; Port: integer);
	var emsg: string; code: longint;
	begin
		Address.Family := af_inet;
		Address.Port := htons(Port);
		Address.Addr := LongWord(StrToNetAddr(Addr));
		if fpBind(Sock, @Address, sizeof(Address)) < 0 then
		begin
			code := SocketError;
			case code of
				nseBadf: emsg := 'The socket descriptor is invalid.';
				nseInval: emsg := 'The socket is already bound to an address.';
				nseAccess: emsg := 'Address is protected and you don''t have permission to open it.';
			end;
			emsg := 'Socket bind error: ' + emsg;
			raise ENetworkSocketException.Create(code, emsg);
		end;
	end;

	constructor TNetworkSocket.Create;
	var emsg: string; code: longint;
	begin
		Sock := fpSocket(AF_INET, SOCK_STREAM, PF_UNSPEC);
		if Sock < 0 then
		begin
			code := SocketError;
			case code of
				nseProtoNoSupport: emsg := 'The protocol type or the specified protocol is not supported within this domain.';
				nseMFile: emsg := 'The per-process descriptor table is full.';
				nseNFile: emsg := 'The system file table is full.';
				nseAccess: emsg := 'Permission to create a socket of the specified type and/or protocol is denied.';
				nseNoBufs: emsg := 'Insufficient buffer space is available. The socket cannot be created until sufficient resources are freed.';
			end;
			emsg := 'Socket creation error: ' + emsg;
			raise ENetworkSocketException.Create(code, emsg);
		end;		
		New(CommonDataBlock);
		New(CommonBuffer);
		CommonBuffer^.Buffer := CommonDataBlock;
	end;

	constructor TNetworkSocket.Create(S: cint; Addr: TInetSockAddr);
	begin
		Sock := S;
		Address := Addr;
		New(CommonDataBlock);
		New(CommonBuffer);
		CommonBuffer^.Buffer := CommonDataBlock;
	end;

	destructor TNetworkSocket.Destroy;
	begin
		Dispose(CommonDataBlock);
		Dispose(CommonBuffer);
		inherited;	
	end;

end.
