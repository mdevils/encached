unit Hashtable;
{ Custom-sized hashtable.
  Last 8 bytes of MD5 are used as the hashing function.
  Collision resolution - chaining: http://en.wikipedia.org/wiki/Hash_table#Separate_chaining }

interface

uses
	SysUtils, Classes, md5;
	
type
	{ Chain in hashtable }
	PHashtableDataItem = ^THashtableDataItem;
	THashtableDataItem = record
		{ Next item in the chain }
		Next: PHashtableDataItem;
		{ Key }
		Key: AnsiString;
		{ Exact value }
		Value: Pointer;
	end;
	
	{ Hashtable cell }
	PHashtableCell = ^THashtableCell;
	THashtableCell = record
		{ Hashtable item }
		Item: PHashtableDataItem;
	end;

	{ Hashtable traversing function definition }
	THashtableTraverserFunction		= procedure (Key: AnsiString; Value: Pointer);
	{ Hashtable traversing method definition }
	THashtableTraverserMethod		= procedure (Key: AnsiString; Value: Pointer) of object;
	{ Wrapper for function/method calling }
	THashtableTraverser = class
		{ Function to call }
		Func: THashtableTraverserFunction;
		{ Method to call }
		Exec: THashtableTraverserMethod;
		{ Constructor }
		constructor Create(HTFunc: THashtableTraverserFunction);
		{ Constructor }
		constructor Create(HTMethod: THashtableTraverserMethod);
		{ Caller }
		procedure ExecFunc(Key: AnsiString; Value: Pointer);
	end;
	
	{ Hashtable }
	THashtable = class
	public
		{ Hash table }
		Table: array of PHashtableCell;
		{ Table length }
		TableLen: longword;
		{ Tabel high: High(Table) }
		TableHigh: longword;
		{ Hashtable clearer }
		Clearer: THashtableTraverser;
		{ Puts value into hashtable }
		procedure Put(Key: AnsiString; Value: Pointer);
		{ Returns value from hashtable }
		function Get(Key: AnsiString): Pointer;
		{ Remove value from hashtable }
		function Remove(Key: AnsiString): boolean;
		{ Clears hashtable using clearer method }
		procedure Clear;
		{ Constructor }
		constructor Create(Size: longword; ClearerProc: THashtableTraverser);
		{ Destructor }
		destructor Destroy; override;
	private
		{ Returns index from string key }
		function Index(Key: AnsiString): LongWord;
		{ Returns last 8 bytes of MD5 hash }
		function Hash(Key: AnsiString): QWord;
		{ Allocates chain in binary tree }
		function ItemPlease(Key: AnsiString; Value: Pointer): PHashtableDataItem;
	end;

implementation 

	procedure THashtableTraverser.ExecFunc(Key: AnsiString; Value: Pointer);
	begin
		Func(Key, Value);
	end; 

	constructor THashtableTraverser.Create(HTFunc: THashtableTraverserFunction);
	begin
		Func := HTFunc;
		Exec := @ExecFunc;
	end;
	constructor THashtableTraverser.Create(HTMethod: THashtableTraverserMethod);
	begin
		Exec := HTMethod;
	end;

	function THashtable.Index(Key: AnsiString): LongWord;
	begin
		result := TableLen * Hash(Key) div High(LongWord);
	end;

	function THashtable.ItemPlease(Key: AnsiString; Value: Pointer): PHashtableDataItem;
	begin
		New(result);
		result^.Next := nil;
		result^.Key := Key;
		result^.Value := Value;	
	end; 

	function THashtable.Hash(Key: AnsiString): QWord;
	var m: array[0..15] of byte;
	begin
		m := MD5String(Key);
		result := PLongword(@m[12])^;
	end; 

	procedure THashtable.Clear;
	var i: longword; CurrentItem, PrevItem: PHashtableDataItem;
	begin
		for i := 0 to TableHigh do
		begin
			if Table[i] <> nil then
			begin
				CurrentItem := Table[I]^.Item;
				while CurrentItem <> nil do
				begin
					Clearer.Exec(CurrentItem^.Key, CurrentItem^.Value);
					PrevItem := CurrentItem;
					CurrentItem := CurrentItem^.Next;
					Dispose(PrevItem);
				end;
			end;
			Dispose(Table[i]);
			Table[i] := nil;
		end;
	end;
	
	constructor THashtable.Create(Size: longword; ClearerProc: THashtableTraverser);
	var i:longword;
	begin
		TableLen := Size;
		SetLength(Table, TableLen);
		TableHigh := TableLen - 1;
		Clearer := ClearerProc;
	end;

	destructor THashtable.Destroy;
	begin
		Clearer.Destroy;
		SetLength(Table, 0);
		inherited;
	end;

	procedure THashtable.Put(Key: AnsiString; Value: Pointer);
	var CurrentItem: PHashtableDataItem; I: longword;
	begin
		I := Index(Key);
		if Table[I] = nil then 
		begin
			New(Table[I]);
			Table[I]^.Item := ItemPlease(Key, Value);
			exit;
		end;
		CurrentItem := Table[I]^.Item;
		while true do
		begin
			if CurrentItem^.Key = Key then
			begin
				CurrentItem^.Value := Value;
				exit;
			end;
			if CurrentItem^.Next = nil then break;
			CurrentItem := CurrentItem^.Next;
		end;
		CurrentItem^.Next := ItemPlease(Key, Value);
	end;

	function THashtable.Get(Key: AnsiString): Pointer;
	var CurrentItem: PHashtableDataItem; I: longword;
	begin
		I := Index(Key);
		if Table[I] = nil then
		begin
			result := nil;
			exit;
		end;
		CurrentItem := Table[I]^.Item;
		while CurrentItem <> nil do
		begin
			if CurrentItem^.Key = Key then
			begin
				result := CurrentItem^.Value;
				exit;
			end;
			CurrentItem := CurrentItem^.Next;
		end;
		result := nil;
	end;

	function THashtable.Remove(Key: AnsiString): boolean;
	var CurrentItem, PrevItem: PHashtableDataItem; I: longword; Cell: PHashtableCell;
	begin
		result := false;
		I := Index(Key);
		if Table[I] = nil then
		begin
			exit;
		end;
		PrevItem := nil;
		CurrentItem := Table[I]^.Item;
		while CurrentItem <> nil do
		begin
			if CurrentItem^.Key = Key then
			begin
				Clearer.Exec(CurrentItem^.Key, CurrentItem^.Value);
				result := true;
				if (CurrentItem = Table[I]^.Item) and (CurrentItem^.Next = nil) then
				begin
					Cell := Table[I];
					Table[I] := nil;
					Dispose(Cell);
				end else
				begin
					if PrevItem = nil then
						Table[I]^.Item := CurrentItem^.Next
					else
						PrevItem^.Next := CurrentItem^.Next;
				end; 
				Dispose(CurrentItem);
				exit;
			end;
			PrevItem := CurrentItem;
			CurrentItem := CurrentItem^.Next;
		end;
	end;

end.
