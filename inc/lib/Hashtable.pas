unit Hashtable;
{ Hashtable based on binary three.
  Last 8 bytes of MD5 are used as the hashing function.
  Collision resolution - chaining: http://en.wikipedia.org/wiki/Hash_table#Separate_chaining }

interface

uses
	SysUtils, Classes, md5;
	
type
	{ Chain in binary three node }
	PHashtableDataItem = ^THashtableDataItem;
	THashtableDataItem = record
		{ Next item in the chain }
		Next: PHashtableDataItem;
		{ Key }
		Key: AnsiString;
		{ Exact value }
		Value: Pointer;
	end;

	{ Binary tree node }
	PHashtableBinaryTreeNode = ^THashtableBinaryTreeNode;
	THashtableBinaryTreeNode = record
		{ Left node (lesser) }
		Less: PHashtableBinaryTreeNode;
		{ Right node (greater) }
		Greater: PHashtableBinaryTreeNode;
		{ Chain }
		Item: PHashtableDataItem;
		{ Hash }
		Hash: QWord;
	end;

	{ Traversing delegates }
	THashtableTraverser     = procedure (Key: AnsiString; Value: Pointer);
	THashtableObjTraverser  = procedure (Key: AnsiString; Value: Pointer);
	THashtableNodeTraverser = procedure (Node: PHashtableBinaryTreeNode) of object;
	
	{ Clearing objects }
	THashtableClearerObject = class
		{ Clearer function }
		Clearer: THashtableTraverser;
		procedure Clear(Node: PHashtableBinaryTreeNode);
	end;
	THashtableObjClearerObject = class
		{ Clearer method }
		Clearer: THashtableObjTraverser;
		procedure Clear(Node: PHashtableBinaryTreeNode);
	end;

	{ Traversing objects }
	THashtableTraverserObject = class
		Traverser: THashtableTraverser;
		procedure Traverse(Node: PHashtableBinaryTreeNode);
	end;
	THashtableObjTraverserObject = class
		Traverser: THashtableObjTraverser;
		procedure Traverse(Node: PHashtableBinaryTreeNode);
	end;
	
	{ Hashtable }
	THashtable = class
	public
		{ Root binary tree node }
		Tree: PHashtableBinaryTreeNode;
		{ Puts value into hashtable }
		procedure Put(Key: AnsiString; Value: Pointer);
		{ Returns value from hashtable }
		function Get(Key: AnsiString): Pointer;
		{ Remove value from hashtable }
		function Remove(Key: AnsiString): Pointer;
		{ Clears hashtable using clearer method }
		procedure Clear(Clearer: THashtableTraverser);
		{ Clears hashtable using clearer method }
		procedure Clear(Clearer: THashtableObjTraverser);
		{ Traverse hashtable }
		procedure Traverse(Traverser: THashtableTraverser);
		{ Traverse hashtable }
		procedure Traverse(Traverser: THashtableObjTraverser);
		{ Constructor }
		constructor Create;
		{ Destructor }
		destructor Destroy; override;
	private
		{ Returns last 8 bytes of MD5 hash }
		function Hash(Key: AnsiString): QWord;
		{ Allocates binary tree node }
		function NodePlease(H: QWord; Item: PHashtableDataItem): PHashtableBinaryTreeNode;
		{ Allocates chain in binary tree }
		function ItemPlease(Key: AnsiString; Value: Pointer): PHashtableDataItem;
		{ Internal traversing method }
		procedure TraversePlease(Node: PHashtableBinaryTreeNode; Traverser: THashtableNodeTraverser);		
	end;

implementation 

	procedure THashtableTraverserObject.Traverse(Node: PHashtableBinaryTreeNode);
	var CurrentItem: PHashtableDataItem;
	begin
		CurrentItem := Node^.Item;
		while CurrentItem <> nil do
		begin
			Traverser(CurrentItem^.Key, CurrentItem^.Value);
			CurrentItem := CurrentItem^.Next;
		end;
	end;

	procedure THashtableClearerObject.Clear(Node: PHashtableBinaryTreeNode);
	var CurrentItem, RemovedItem: PHashtableDataItem;
	begin
		CurrentItem := Node^.Item;
		while CurrentItem <> nil do
		begin
			Clearer(CurrentItem^.Key, CurrentItem^.Value);
			RemovedItem := CurrentItem;
			CurrentItem := CurrentItem^.Next;
			Dispose(RemovedItem);
		end;
		Dispose(Node);
	end; 

	procedure THashtableObjTraverserObject.Traverse(Node: PHashtableBinaryTreeNode);
	var CurrentItem: PHashtableDataItem;
	begin
		CurrentItem := Node^.Item;
		while CurrentItem <> nil do
		begin
			Traverser(CurrentItem^.Key, CurrentItem^.Value);
			CurrentItem := CurrentItem^.Next;
		end;
	end;

	procedure THashtableObjClearerObject.Clear(Node: PHashtableBinaryTreeNode);
	var CurrentItem, RemovedItem: PHashtableDataItem;
	begin
		CurrentItem := Node^.Item;
		while CurrentItem <> nil do
		begin
			Clearer(CurrentItem^.Key, CurrentItem^.Value);
			RemovedItem := CurrentItem;
			CurrentItem := CurrentItem^.Next;
			Dispose(RemovedItem);
		end;
		Dispose(Node);
	end;
	
	procedure THashtable.TraversePlease(Node: PHashtableBinaryTreeNode; Traverser: THashtableNodeTraverser);
	var Greater: PHashtableBinaryTreeNode;
	begin
		if Node^.Less <> nil then TraversePlease(Node^.Less, Traverser);
		Greater := Node^.Greater;
		Traverser(Node);
		if Greater <> nil then TraversePlease(Greater, Traverser);
	end; 

	function THashtable.NodePlease(H: QWord; Item: PHashtableDataItem): PHashtableBinaryTreeNode;
	begin
		New(result);
		result^.Less := nil;
		result^.Greater := nil;
		result^.Hash := H;
		result^.Item := Item;
	end; 

	function THashtable.ItemPlease(Key: AnsiString; Value: Pointer): PHashtableDataItem;
	begin
		New(result);
		result^.Key := Key;
		result^.Value := Value;
		result^.Next := nil;		
	end; 

	function THashtable.Hash(Key: AnsiString): QWord;
	var m: array[0..15] of byte;
	begin
		m := MD5String(Key);
		result := PQWord(@m[8])^;
	end; 

	procedure THashtable.Clear(Clearer: THashtableTraverser);
	var ClearObj: THashtableClearerObject;
	begin
		if Tree = nil then exit;
		ClearObj := THashtableClearerObject.Create;
		ClearObj.Clearer := Clearer;
		TraversePlease(Tree, @ClearObj.Clear);
		Tree := nil;
	end;

	procedure THashtable.Clear(Clearer: THashtableObjTraverser);
	var ClearObj: THashtableObjClearerObject;
	begin
		if Tree = nil then exit;
		ClearObj := THashtableObjClearerObject.Create;
		ClearObj.Clearer := Clearer;
		TraversePlease(Tree, @ClearObj.Clear);
		Tree := nil;
	end;
	
	procedure THashtable.Traverse(Traverser: THashtableObjTraverser);
	var TraverserObj: THashtableTraverserObject;
	begin
		if Tree = nil then exit;
		TraverserObj := THashtableTraverserObject.Create;
		TraverserObj.Traverser := Traverser;
		TraversePlease(Tree, @TraverserObj.Traverse);
	end; 

	procedure THashtable.Traverse(Traverser: THashtableTraverser);
	var TraverserObj: THashtableObjTraverserObject;
	begin
		if Tree = nil then exit;
		TraverserObj := THashtableObjTraverserObject.Create;
		TraverserObj.Traverser := Traverser;
		TraversePlease(Tree, @TraverserObj.Traverse);	
	end;
	
	constructor THashtable.Create;
	begin
		Tree := nil;
	end; 

	destructor THashtable.Destroy;
	begin
		Dispose(Tree);
		inherited;
	end;

	procedure THashtable.Put(Key: AnsiString; Value: Pointer);
	var CurrentNode: PHashtableBinaryTreeNode; CurrentItem: PHashtableDataItem; H, CH: QWord;
	begin
		H := Hash(Key);
		if Tree = nil then
		begin
			Tree := NodePlease(H, ItemPlease(Key, Value));
		end;
		CurrentNode := Tree;
		while true do
		begin
			CH := CurrentNode^.Hash;
			if H = CH then
			begin
				CurrentItem := CurrentNode^.Item;
				while true do
				begin
					if CurrentItem^.Key = Key then
					begin
						CurrentItem^.Value := Value;
						exit;
					end;
					if CurrentItem^.Next = nil then break;
				end;
				CurrentItem^.Next := ItemPlease(Key, Value);
				exit;
			end else
			if H < CH then
			begin
				if CurrentNode^.Less = nil then
				begin
					CurrentNode^.Less := NodePlease(H, ItemPlease(Key, Value));
					exit;
				end
				else CurrentNode := CurrentNode^.Less;
			end else
				if CurrentNode^.Greater = nil then
				begin
					CurrentNode^.Greater := NodePlease(H, ItemPlease(Key, Value));
					exit;
				end else CurrentNode := CurrentNode^.Greater;
		end;		
	end;

	function THashtable.Get(Key: AnsiString): Pointer;
	var CurrentNode: PHashtableBinaryTreeNode; CurrentItem: PHashtableDataItem; H, CH: QWord;
	begin
		H := Hash(Key);
		CurrentNode := Tree;
		result := nil;
		while CurrentNode <> nil do
		begin
			CH := CurrentNode^.Hash;
			if H = CH then
			begin
				CurrentItem := CurrentNode^.Item;
				while CurrentItem <> nil do
				begin
					if CurrentItem^.Key = Key then
					begin
						result := CurrentItem^.Value;
						exit;
					end;
					CurrentItem := CurrentItem^.Next;
				end;
				exit;
			end
			else
			if H < CH then CurrentNode := CurrentNode^.Less
			else CurrentNode := CurrentNode^.Greater;
		end;
	end;

	function THashtable.Remove(Key: AnsiString): Pointer;
	var ParentNode, CurrentNode, MinNode: PHashtableBinaryTreeNode; CurrentItem, PrevItem: PHashtableDataItem; H, CH: QWord;
	begin
		H := Hash(Key);
		CurrentNode := Tree;
		result := nil;
		ParentNode := nil;
		while CurrentNode <> nil do
		begin
			CH := CurrentNode^.Hash;
			if H = CH then
			begin
				PrevItem := nil;
				CurrentItem := CurrentNode^.Item;
				while CurrentItem <> nil do
				begin
					if CurrentItem^.Key = Key then
					begin
						result := CurrentItem^.Value;
						if (CurrentItem = CurrentNode^.Item) and (CurrentItem^.Next = nil) then
						begin
							if CurrentNode^.Less = nil then
							begin
								if ParentNode = nil then Tree := CurrentNode^.Greater
								else if ParentNode^.Less = CurrentNode then ParentNode^.Less := CurrentNode^.Greater
								else ParentNode^.Greater := CurrentNode^.Greater;
							end
							else if CurrentNode^.Greater = nil then
							begin
								if ParentNode = nil then Tree := CurrentNode^.Less
								else if ParentNode^.Greater = CurrentNode then ParentNode^.Greater := CurrentNode^.Less
								else ParentNode^.Less := CurrentNode^.Less;
							end
							else
							begin
								MinNode := CurrentNode^.Greater;
								while MinNode^.Less <> nil do
								begin
									MinNode := MinNode^.Less;
								end;
								MinNode^.Less := CurrentNode^.Less;
								if ParentNode = nil then Tree := CurrentNode^.Greater
								else if ParentNode^.Less = CurrentNode then ParentNode^.Less := CurrentNode^.Greater
								else ParentNode^.Greater := CurrentNode^.Greater;
							end;
							Dispose(CurrentNode);
						end else
						begin
							if PrevItem = nil then
								CurrentNode^.Item := CurrentItem^.Next
							else
								PrevItem^.Next := CurrentItem^.Next;
						end; 
						Dispose(CurrentItem);
						exit;
					end;
					PrevItem := CurrentItem;
					CurrentItem := CurrentItem^.Next;
				end;
				exit;
			end
			else begin
				ParentNode := CurrentNode;
				if H < CH then CurrentNode := CurrentNode^.Less
				else CurrentNode := CurrentNode^.Greater;
			end;
		end;
	end;

	
end.
