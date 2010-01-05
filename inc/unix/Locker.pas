unit Locker;

interface

type
	{ Simple locker based on CriticalSections }
	TLocker = class
		{ Critical section }
		InnerLock: TRTLCriticalSection;
	public
		{ Constructor }
		constructor Create;
		{ Lock }
		procedure Lock;
		{ Unlock }
		procedure Unlock;
		{ Destructor }
		destructor Destroy; override;
	end;

implementation

	constructor TLocker.Create;
	begin
		InitCriticalSection(InnerLock);
	end; 

	procedure TLocker.Lock;
	begin
		EnterCriticalSection(InnerLock);
	end; 

	procedure TLocker.Unlock;
	begin
		LeaveCriticalSection(InnerLock);
	end; 

	destructor TLocker.Destroy;
	begin
		DoneCriticalSection(InnerLock);
		inherited Destroy;
	end;

end.