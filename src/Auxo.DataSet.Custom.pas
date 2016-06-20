unit Auxo.DataSet.Custom;

interface

uses
  Data.DB, System.Generics.Collections, System.TypInfo, System.SysUtils, System.Classes, System.Rtti;

type
  TAuxoValueBuffer = TValueBuffer;
  TAuxoBookmark = TBookmark;
  TAuxoRecordBuffer = TRecBuf;

  TCustomAuxoDataset = class(TDataSet)
  strict private
    type
      PRecInfo = ^TRecInfo;
      ValueType = Variant;

      TRecInfo = packed record
        Index: integer;
        Data: TValue;
        Bookmark: Integer;
        BookmarkFlag: TBookmarkFlag;
      end;

      PValueList = ^TValueList;
      TValueList = array[0..0] of ValueType;
  strict private
    // Variables used for paged cursor
    FDeleteMode: Boolean;
    //FRemainingRows: integer;
    //FCurrentFirstRow: integer;
    //FPageSize: integer;
    FCurrent: integer;
    FIsOpen: boolean;
    FRecBufSize: integer;
    FRecInfoOffset: integer;
    FRecordSize: integer;
    FOldValueBuffer: TAuxoRecordBuffer;
    FFilterBuffer: TAuxoRecordBuffer;
    FModifiedFields: TList<TField>;
    //FKeyMapping: TDictionary<Variant, Integer>;
    function GetBufferValueList(Buffer: TAuxoRecordBuffer): PValueList;
    function IsSelfField(Field: TField): boolean;
    function GetBufferRecInfo(Buffer: TAuxoRecordBuffer): PRecInfo;
//    procedure UpdateListFromParent(Field: TDatasetField);//TODO
    function InternalGetRecord(Buffer: TAuxoRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult;
    procedure FetchAllRecords;
    procedure GetTheFieldList(List: TList<TField>; const FieldNames: string);
    function GetFieldVariant(Field: TField; var Data: ValueType): boolean;
  private
    const NullBuffer: TAuxoRecordBuffer = 0;
    const SelfFieldName = 'Self';
  private
    FKeyField: string;
    function GetActiveRecBuf: TAuxoRecordBuffer;
    procedure LoadFieldDefsFromFields(AFields: TFields; AFieldDefs: TFieldDefs);
    function MatchField(Field: TField; Value: Variant; Options: TLocateOptions): Boolean;
    function MatchRecord(FieldList: TList<TField>; const KeyValues: Variant; Options: TLocateOptions): boolean;
  protected
    function DoFetchMoreRecords: boolean; virtual; abstract;
    function DoFetchingRecords: boolean; virtual; abstract;
    function DoNewRecord: TValue; virtual; abstract;
    function DoRecordToData(Rec: TValue): Variant; virtual; abstract;
    function DoGetRecord(AIndex: Integer): TValue; virtual; abstract;
    function DoRecordInsert(ARecord: TValue; AIndex: Integer): Boolean; virtual; abstract;
    function DoRecordUpdate(ARecord: TValue; ARecordIndex: Integer = 0): Boolean; virtual; abstract;
    function DoRecordDelete(ARecord: Integer): Boolean; virtual; abstract;

    function DoRecordUpdateField(Field: TField; Rec: TValue; Value: Variant): Boolean; virtual; abstract;
    function DoGetFieldValue(Field: TField; Rec: TValue): Variant; virtual; abstract;
    function DoLocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean; ResultFields: string): Boolean; virtual;
    function DoListCount: integer; virtual; abstract;
  protected
    //procedure CreateFields; override;//TODO
    //function CreateNestedDataSet(DataSetField: TDataSetField): TDataSet; override;//TODO
    procedure DataEvent(Event: TDataEvent; Info: NativeInt); override;
    function GetCanModify: boolean; override;
    function GetRecordCount: integer; override;
    function GetRecNo: Longint; override;
    //procedure SetDataSetField(const Value: TDataSetField); override;//TODO
    procedure SetRecNo(Value: integer); override;
    procedure DoAfterOpen; override;
    procedure SetFiltered(Value: Boolean); override;
    procedure DoOnNewRecord; override;
    procedure InternalEdit; override;
  protected
    function AllocRecBuf: TRecBuf; {$IFDEF NEXTGEN} override; {$ENDIF}
    procedure FreeRecBuf(var Buffer: TRecBuf); {$IFDEF NEXTGEN} override; {$ENDIF}
    function AllocRecordBuffer: TRecordBuffer; {$IFNDEF NEXTGEN} override; {$ENDIF}
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); {$IFNDEF NEXTGEN} override; {$ENDIF}


    procedure GetBookmarkData(Buffer: TAuxoRecordBuffer; Data: TAuxoBookmark); override;
    function GetBookmarkFlag(Buffer: TAuxoRecordBuffer): TBookmarkFlag; override;
    function GetRecordSize: Word; override;
    procedure InternalDelete; override;
    procedure InternalFirst; override;
//    procedure InternalGotoBookmark(Bookmark: TAuxoBookmark); override;
    procedure InternalInitRecord(Buffer: TAuxoRecordBuffer); override;
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure UpdateRecordFields(Obj: TValue; RecBuf: TAuxoRecordBuffer);
    procedure InternalLast; override;
    procedure InternalPost; override;

    procedure InternalSetToRecord(Buffer: TAuxoRecordBuffer); override;
    procedure SetBookmarkFlag(Buffer: TAuxoRecordBuffer; Value: TBookmarkFlag); override;
    function FieldBufferToVariant(Field: TField; Buffer: TAuxoValueBuffer; NativeFormat: boolean): Variant;
    procedure SetFieldData(Field: TField; Buffer: TAuxoValueBuffer); overload; override;
    procedure SetFieldData(Field: TField; Buffer: TAuxoValueBuffer; NativeFormat: boolean); override;
  protected
    function GetRecord(Buffer: TAuxoRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult; override;
    procedure InternalClose; override;
    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: boolean; override;
    procedure VariantToFieldBuffer(Field: TField; Data: ValueType; var Buffer: TAuxoValueBuffer);
  protected
    procedure SetBookmarkData(Buffer: NativeInt; Data: TArray<Byte>); override;
    procedure InternalInsert; override;
    procedure InternalCancel; override;
    procedure InternalGotoBookmark(Bookmark: System.TArray<Byte>); override;
    property KeyField: string read FKeyField write FKeyField;
    //procedure InternalSetSourceList(SourceList: TObject);
    //procedure InitFieldDefsFromClass(AClass: TClass);
  public
    //function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    function GetFieldData(Field: TField; var Buffer: TAuxoValueBuffer): boolean; override;
    function IsSequenced: Boolean; override;
    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): boolean; override;
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
    property DeleteMode: Boolean read FDeleteMode write FDeleteMode;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses
  System.Variants, Data.FmtBcd, System.Types, Data.DBConsts;

function TCustomAuxoDataset.GetBufferValueList(Buffer: TAuxoRecordBuffer): PValueList;
begin
  Result := PValueList(Buffer);
end;

function TCustomAuxoDataset.IsSelfField(Field: TField): boolean;
begin
  Result := Field.FieldName = SelfFieldName;
end;

function TCustomAuxoDataset.GetBufferRecInfo(Buffer: TAuxoRecordBuffer): PRecInfo;
begin
  Result := PRecInfo(Buffer + FRecInfoOffset);
end;

function TCustomAuxoDataset.InternalGetRecord(Buffer: TAuxoRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult;
begin
  try
    Result := grOK;
    case GetMode of
      gmNext:
        Inc(FCurrent);
      gmPrior:
        Dec(FCurrent);
    end;
    if FCurrent < 0 then
    begin
      Result := grBOF;
      FCurrent := -1;
    end
    else if (FCurrent >= DoListCount) then
    begin
      if not DoFetchMoreRecords then
      begin
        Result := grEOF;
        if DoListCount = 0 then
          FCurrent := -1
        else
          FCurrent := DoListCount;
      end;
    end;

    if Result = grOK then
    begin
      with GetBufferRecInfo(Buffer)^ do
      begin
        Bookmark := FCurrent;
        Index := FCurrent;
        Data := DoGetRecord(FCurrent);
        BookmarkFlag := bfCurrent;
      end;
      Finalize(GetBufferValueList(Buffer)^, Fields.Count);
      GetCalcFields(Buffer);
    end;
  except
    Result := grError;
    if DoCheck then
      raise ;
  end;
end;

procedure TCustomAuxoDataset.InternalGotoBookmark(Bookmark: TArray<System.Byte>);
begin
  FCurrent := PInteger(Bookmark)^;
end;

procedure TCustomAuxoDataset.GetTheFieldList(List: TList<TField>; const FieldNames: string);
begin
  GetFieldList(List, FieldNames);
end;

procedure TCustomAuxoDataset.FetchAllRecords;
begin
  while DoFetchMoreRecords do;
end;

function TCustomAuxoDataset.GetFieldVariant(Field: TField; var Data: ValueType): boolean;
var
  RecBuf: TAuxoRecordBuffer;
  Rec: TValue;
begin
  RecBuf := GetActiveRecBuf;
  if RecBuf = NullBuffer then
    Exit(false);
  Data := GetBufferValueList(RecBuf)[Field.Index];

  if Field.FieldName = '$INDEX' then
    Data := GetBufferRecInfo(RecBuf)^.Index;
  if Field.FieldName = '$BUFFERCOUNT' then
    Data := FRecBufSize;
  if Field.FieldName = '$CURRENT' then
    Data := FCurrent;
  if Field.FieldName = '$OFFSET' then
    Data := FRecInfoOffset;
  if Field.FieldName = '$RECORDSIZE' then
    Data := FRecordSize;
  if VarIsEmpty(Data) then
  begin
    Rec := GetBufferRecInfo(RecBuf)^.Data;
    if IsSelfField(Field) then
      Data := DoRecordToData(Rec)
    else
    begin
      if Field.FieldKind = fkData then
        Data := DoGetFieldValue(Field, Rec);
    end;
    if VarIsEmpty(Data) then
      Data := Null;
    GetBufferValueList(RecBuf)[Field.Index] := Data;
  end;
  Result := not VarIsNull(Data);
end;

function TCustomAuxoDataset.GetActiveRecBuf: TAuxoRecordBuffer;
begin
  case State of
    dsBlockRead, dsBrowse:
      if IsEmpty then
        Result := NullBuffer
      else
        Result := ActiveBuffer;
    dsCalcFields, dsInternalCalc:
      Result := CalcBuffer;
    dsFilter:
      Result := FFilterBuffer;
    dsOldValue:
      if FOldValueBuffer <> NullBuffer then
        Result := FOldValueBuffer
      else
        Result := ActiveBuffer;
    dsEdit, dsInsert, dsNewValue: Result := ActiveBuffer;
  else
    Result := NullBuffer;
  end;
end;

procedure TCustomAuxoDataset.DataEvent(Event: TDataEvent; Info: NativeInt);
begin
  case Event of
    deParentScroll:
      if DataSetField <> nil then
      begin
        //UpdateListFromParent(DataSetField);//TODO
        Resync([]);
      end;
  end;
  inherited;
end;

function TCustomAuxoDataset.GetCanModify: boolean;
begin
  Result := not FDeleteMode;
end;

function TCustomAuxoDataset.GetRecordCount: integer;
begin
  if Active then
  begin
    if DoFetchingRecords then
      Result := -1
    else
      Result := DoListCount;
  end
  else
    Result := 0;
end;

function TCustomAuxoDataset.GetRecNo: Longint;
var
  RecBuf: TAuxoRecordBuffer;
begin
  CheckActive;
  Result := -1;
  RecBuf := GetActiveRecBuf;
  if (RecBuf <> NullBuffer) and (GetBufferRecInfo(RecBuf)^.BookmarkFlag = bfCurrent) then
    Result := GetBufferRecInfo(RecBuf)^.Index + 1;
end;

procedure TCustomAuxoDataset.SetRecNo(Value: integer);
begin
  CheckBrowseMode;
  if Value < 1 then
    Value := 1;
  if Value > DoListCount then
  begin
    while DoFetchingRecords and (Value > DoListCount) do
      DoFetchMoreRecords;
    if Value > DoListCount then
      Value := DoListCount;
  end;
  if RecNo <> Value then
  begin
    DoBeforeScroll;
    FCurrent := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

procedure TCustomAuxoDataset.DoAfterOpen;
var
  I: Integer;
begin
  // Notify nested datasets to be refreshed
  RecNo := 0;
  if Assigned(NestedDataSets) then
  begin
    for I := 0 to NestedDataSets.Count - 1 do
      if TObject(NestedDatasets[I]) is TCustomAuxoDataset then
      begin
        with TCustomAuxoDataset(NestedDataSets[I]) do
        begin
          if Active then
          begin
            //UpdateListFromParent(DatasetField);  //TODO
            Resync([]);
          end;
        end;
      end;
  end;
  inherited;
end;

procedure TCustomAuxoDataset.SetFiltered(Value: Boolean);
begin
  if Value <> Filtered then
  begin
    inherited;
    if Active then
    begin
      if Filtered then
        First
      else
        Refresh;
    end;
  end;
end;

procedure TCustomAuxoDataset.DoOnNewRecord;
begin
  FModifiedFields.Clear;
  inherited;
end;

procedure TCustomAuxoDataset.InternalEdit;
begin
  FModifiedFields.Clear;
  DeleteMode := False;
  inherited;
end;

procedure TCustomAuxoDataset.GetBookmarkData(Buffer: TAuxoRecordBuffer; Data: TAuxoBookmark);
begin
  inherited;
  PInteger(Data)^ := GetBufferRecInfo(Buffer)^.Bookmark;
end;

function TCustomAuxoDataset.GetBookmarkFlag(Buffer: TAuxoRecordBuffer): TBookmarkFlag;
begin
  Result := GetBufferRecInfo(Buffer)^.BookmarkFlag;
end;

function TCustomAuxoDataset.GetRecordSize: Word;
begin
  Result := FRecordSize;
end;

procedure TCustomAuxoDataset.InternalDelete;
begin
  DeleteMode := False;
  if DoRecordDelete(FCurrent) then
  begin
    if FCurrent >= DoListCount then
      FCurrent := DoListCount - 1;
  end;
end;

procedure TCustomAuxoDataset.InternalFirst;
begin
  FCurrent := -1;
end;

procedure TCustomAuxoDataset.InternalInitFieldDefs;
begin
  FieldDefs.Clear;
  LoadFieldDefsFromFields(Fields, FieldDefs);
end;

procedure TCustomAuxoDataset.InternalInitRecord(Buffer: TRecordBuffer);
begin
  InternalInitRecord(TAuxoRecordBuffer(Buffer));
end;

procedure TCustomAuxoDataset.InternalInitRecord(Buffer: TAuxoRecordBuffer);
var
  I: integer;
begin
  for I := 0 to Fields.Count - 1 do
    GetBufferValueList(Buffer)[I] := Null;
end;

procedure TCustomAuxoDataset.InternalInsert;
begin
  inherited;
  DeleteMode := False;
end;

procedure TCustomAuxoDataset.InternalLast;
begin
  FetchAllRecords;
  FCurrent := DoListCount;
end;

procedure TCustomAuxoDataset.UpdateRecordFields(Obj: TValue; RecBuf: TAuxoRecordBuffer);
var
  Field: TField;
  Value: ValueType;
begin
  for Field in FModifiedFields do
  begin
    if (Field.FieldKind = fkData) and (TProviderFlag.pfInUpdate in Field.ProviderFlags) then
    begin
      Value := GetBufferValueList(RecBuf)[Field.Index];
      DoRecordUpdateField(Field, Obj, Value);
    end;
  end;
end;

procedure TCustomAuxoDataset.InternalPost;
var
  RecBuf: TAuxoRecordBuffer;
var
  Obj: TValue;
  InsertIndex: Integer;
begin
  inherited;
  UpdateCursorPos;
  RecBuf := GetActiveRecBuf;
  if State = dsEdit then
  begin
    Obj := DoNewRecord;
    UpdateRecordFields(Obj, RecBuf);
    DoRecordUpdate(Obj, FCurrent);
    Obj := DoGetRecord(FCurrent);
    UpdateRecordFields(Obj, RecBuf);
  end
  else
  begin
//    case GetBufferRecInfo(RecBuf)^.BookmarkFlag of
//      bfBOF:
//        InsertIndex := 0;
//      bfEOF:
//        InsertIndex := -1;
//    else
//      InsertIndex := 0;
//    end;
    InsertIndex := 0;
    Obj := DoNewRecord;
    UpdateRecordFields(Obj, RecBuf);
    FetchAllRecords;
    DoRecordInsert(Obj, InsertIndex);
  end;
end;

procedure TCustomAuxoDataset.InternalSetToRecord(Buffer: TAuxoRecordBuffer);
begin
  if GetBufferRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    FCurrent := GetBufferRecInfo(Buffer)^.Bookmark;
end;

procedure TCustomAuxoDataset.SetBookmarkFlag(Buffer: TAuxoRecordBuffer; Value: TBookmarkFlag);
begin
  GetBufferRecInfo(Buffer)^.BookmarkFlag := Value;
end;

procedure TCustomAuxoDataset.SetBookmarkData(Buffer: NativeInt; Data: System.TArray<System.Byte>);
begin
  if PRecInfo(Buffer)^.BookmarkFlag in [bfCurrent, bfInserted] then
    PRecInfo(Buffer)^.Bookmark := PInteger(Data)^
  else
    PRecInfo(Buffer)^.Bookmark := -1;
end;

procedure TCustomAuxoDataset.SetFieldData(Field: TField; Buffer: TAuxoValueBuffer);
begin
  SetFieldData(Field, Buffer, True);
end;

function TCustomAuxoDataset.FieldBufferToVariant(Field: TField; Buffer: TAuxoValueBuffer; NativeFormat: boolean): Variant;
var
  SInt: Smallint;
  I: Integer;
  LI: LargeInt;
  B: Boolean;
  F: Double;
  BCD: TBCD;
  TempValue: TValueBuffer;
  NullIndex: integer;
  Str: string;
begin
  case Field.DataType of
    ftString, ftFixedChar:
      begin
        Str := TEncoding.ANSI.GetString(Buffer);
        NullIndex := Str.IndexOf(#0);
        if NullIndex >= 0 then
          Result := Str.Remove(NullIndex)
        else
          Result := Str;
      end;
    ftWideString, ftFixedWideChar:
      begin
        Result := WideString(PWideChar(Buffer));
      end;
    ftSmallint:
      begin
        SInt := TBitConverter.InTo<SmallInt>(Buffer);
        Result := SInt;
      end;
    ftInteger:
      begin
        I := TBitConverter.InTo<Integer>(Buffer);
        Result := I;
      end;
    ftLargeint:
      begin
        LI := TBitConverter.InTo<Largeint>(Buffer);
        Result := LI;
      end;
    ftBoolean:
      begin
        B := TBitConverter.InTo<Boolean>(Buffer);
        Result := B;
      end;
    ftFloat, ftCurrency:
      begin
        F := TBitConverter.InTo<Double>(Buffer);
        Result := F;
      end;
    ftFmtBCD:
      begin
        BCD := TBitConverter.InTo<TBCD>(Buffer);
        VarFMTBcdCreate(Result, BCD);
      end;
    ftDate, ftTime, ftDateTime:
      begin
        if NativeFormat then
        begin
          SetLength(TempValue, SizeOf(TVarData(Result).VDate));
          DataConvert(Field, Buffer, TempValue, False);
          TVarData(Result).VDate := TBitConverter.InTo<Double>(TempValue);
        end else
          Result := TDateTime(TBitConverter.InTo<Double>(Buffer));
      end;
//    ftBlob, ftGraphic: //TODO
//      begin
//        Result := TUtils.BytesToVariant(TBytes(Buffer));
//      end;
    ftMemo:
      begin
        Str := TEncoding.ANSI.GetString(Buffer);
        NullIndex := Str.IndexOf(#0);
        if NullIndex >= 0 then
          Result := Str.Remove(NullIndex)
        else
          Result := Str;
      end;
    ftWideMemo:
      begin
        Result := WideStringOf(TBytes(Buffer));
      end;
    ftVariant:
      begin
        Result := Variant(PVariant(@Buffer[0])^);
      end;
    ftGuid:
      begin
        Str := TEncoding.ANSI.GetString(Buffer);
        NullIndex := Str.IndexOf(#0);
        if NullIndex >= 0 then
          Result := Str.Remove(NullIndex)
        else
          Result := Str;
      end;
  end;
end;


procedure TCustomAuxoDataset.FreeRecBuf(var Buffer: TRecBuf);
begin
  FreeRecordBuffer(TRecordBuffer(Buffer));
end;

procedure TCustomAuxoDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  PRecInfo(Buffer + FRecInfoOffset)^.Data := nil;
  FreeMem(Buffer);
  Buffer := nil;
end;

procedure TCustomAuxoDataset.SetFieldData(Field: TField; Buffer: TAuxoValueBuffer; NativeFormat: boolean);
var
  RecBuf: TAuxoRecordBuffer;
  Data: ValueType;
begin
  if not(State in dsWriteModes) then
    DatabaseError(SNotEditing, Self);
  if IsSelfField(Field) then
    Exit;

  RecBuf := GetActiveRecBuf;
  if RecBuf = NullBuffer then
    Exit;

  if Field.ReadOnly and not(State in [dsSetKey, dsFilter]) then
    DatabaseErrorFmt(SFieldReadOnly, [Field.DisplayName]);
  Field.Validate(Buffer);
  if FModifiedFields.IndexOf(Field) = -1 then
    FModifiedFields.Add(Field);

  if Buffer = nil then
    Data := Null
  else
    Data := FieldBufferToVariant(Field, Buffer, NativeFormat);
  GetBufferValueList(RecBuf)[Field.Index] := Data;
  if not(State in [dsCalcFields, dsInternalCalc, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, IntPtr(Field));
end;

function TCustomAuxoDataset.GetRecord(Buffer: TAuxoRecordBuffer; GetMode: TGetMode; DoCheck: boolean): TGetResult;
var
  SaveState: TDataSetState;
  AcceptRecord: Boolean;
begin
  if Filtered and Assigned(OnFilterRecord) then
  begin
    FFilterBuffer := Buffer;
    SaveState := SetTempState(dsFilter);
    try
      AcceptRecord := True;
      repeat
        Result := InternalGetRecord(Buffer, GetMode, DoCheck);
        if Result = grOK then
        begin
          OnFilterRecord(Self, AcceptRecord);
          if not AcceptRecord and (GetMode = gmCurrent) then
            Result := grError;
        end;
      until AcceptRecord or (Result <> grOK);
    except
      InternalHandleException;
      Result := grError;
    end;
    RestoreState(SaveState);
  end
  else
    Result := InternalGetRecord(Buffer, GetMode, DoCheck)
end;

procedure TCustomAuxoDataset.InternalCancel;
begin
  DeleteMode := False;
  inherited;
end;

procedure TCustomAuxoDataset.InternalClose;
begin
  FIsOpen := false;
  BindFields(false);

  if (TFieldLifeCycle.lcAutomatic in Fields.LifeCycles) then
    DestroyFields;
  FCurrent := -1;
end;

procedure TCustomAuxoDataset.InternalHandleException;
begin
  if Assigned(System.Classes.ApplicationHandleException) then
    System.Classes.ApplicationHandleException(Self);
end;

procedure TCustomAuxoDataset.InternalOpen;
begin
//  if DataSetField <> nil then//TODO
//    UpdateListFromParent(DataSetField);//TODO
  FCurrent := -1;
  InternalInitFieldDefs;
  if (TFieldLifeCycle.lcAutomatic in Fields.LifeCycles) then
    CreateFields;
  BindFields(True);

  // Initialize buffer pointers
  FRecordSize := Fields.Count * SizeOf(ValueType);
  FRecBufSize := FRecordSize + SizeOf(TRecInfo);
  FRecInfoOffset := FRecordSize;

  // create internal buffers
  FIsOpen := True;
end;

function TCustomAuxoDataset.IsCursorOpen: boolean;
begin
  Result := FIsOpen;
end;

procedure TCustomAuxoDataset.VariantToFieldBuffer(Field: TField; Data: ValueType; var Buffer: TAuxoValueBuffer);
var
  Str: string;
  SInt: Smallint;
  I: integer;
  I64: LargeInt;
  B: boolean;
  F: double;
  BCD: TBCD;
  D: TDateTime;
  TempValue: TValueBuffer;
  M: TMarshaller;
begin
  case Field.DataType of
    ftString, ftFixedChar:
      begin
        FillChar(Buffer[0], Field.DataSize, 0);
        Str := VarToStr(Data);
        TMarshal.Copy(M.AsAnsi(Str), Buffer, 0, Field.DataSize);
      end;
    ftWideString, ftFixedWideChar:
      begin
        FillChar(Buffer[0], Field.DataSize, 0);
        Str := VarToStr(Data);
        StrLCopy(PChar(Buffer), PChar(Str), Field.Size);
      end;
    ftSmallint:
      begin
        SInt := Data;
        TBitConverter.From<SmallInt>(SInt, Buffer);
      end;
    ftInteger:
      begin
        I := Data;
        TBitConverter.From<Integer>(I, Buffer);
      end;
    ftLargeint:
      begin
        I64 := Data;
        TBitConverter.From<LargeInt>(I64, Buffer);
      end;
    ftBoolean:
      begin
        B := Data;
        TBitConverter.From<Boolean>(B, Buffer);
      end;
    ftFloat, ftCurrency:
      begin
        F := Data;
        TBitConverter.From<Double>(F, Buffer);
      end;
    ftFmtBCD:
      begin
        BCD := VarToBcd(Data);
        TBitConverter.From<TBcd>(BCD, Buffer);
      end;
    ftDate, ftTime, ftDateTime:
      begin
        D := VarToDateTime(Data);
        SetLength(TempValue, SizeOf(double));
        TBitConverter.From<Double>(D, TempValue);
        DataConvert(Field, TempValue, Buffer, True);
      end;
    ftBlob, ftMemo, ftWideMemo, ftGraphic:
      begin
         // Not needed, blob stream uses GetBlobData directly
         // TempBytes := TUtils.VariantToBytes(Data);              //j¡ ESTAVA ASSIM
         // System.Move(TempBytes[0], Buffer^, Length(TempBytes));//j¡ ESTAVA ASSIM
         // Variant(Buffer^) := Data;//j¡ ESTAVA ASSIM
      end;
    ftVariant:
      begin
        Variant(PVariant(@Buffer[0])^) := Data;
      end;
    ftGuid:
      begin
        // Same as string
        FillChar(Buffer[0], Field.DataSize * SizeOf(Char), 0);
        Str := VarToStr(Data);
        TMarshal.Copy(M.AsAnsi(Str), Buffer, 0, Field.DataSize);
      end
  else
    // ftWord,ftBytes,ftVarBytes,ftAutoInc,ftGraphic,ftFmtMemo,ftParadoxOle,ftDBaseOle,ftTypedBinary,
    // ftCursor,ftADT,ftArray,ftReference,ftDataSet,ftOraBlob,ftOraClob,ftVariant,ftInterface,ftIDispatch,
    // ftGuid,ftTimeStamp,ftFMTBcd,ftOraTimeStamp,ftOraInterval,ftLongWord,ftShortint,ftByte,ftExtended,
    // ftConnection,ftParams,ftStream,ftTimeStampOffset,ftObject,ftSingle,
  end;
end;

function TCustomAuxoDataset.GetFieldData(Field: TField; var Buffer: TAuxoValueBuffer): boolean;
var
  Data: ValueType;
begin
  Result := GetFieldVariant(Field, Data);
  if Length(Buffer) = 0 then
    Exit;
  if Result and (Buffer <> nil) then
    VariantToFieldBuffer(Field, Data, Buffer);
end;

function TCustomAuxoDataset.IsSequenced: Boolean;
begin
  if not Active then
    Exit(False);
  Result := not DoFetchingRecords;
end;

function TCustomAuxoDataset.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): boolean;
begin
  DoBeforeScroll;
  Result := DoLocateRecord(KeyFields, KeyValues, Options, True, '');
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TCustomAuxoDataset.Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant;
begin
  VarClear(Result);
  if DoLocateRecord(KeyFields, KeyValues, [], False, ResultFields) then
  begin
    SetTempState(dsFilter);
    try
      Result := FieldValues[ResultFields];
    finally
      RestoreState(dsBrowse);
    end;
  end;
end;

function TCustomAuxoDataset.AllocRecBuf: TRecBuf;
begin
  Result := TRecBuf(AllocRecordBuffer);
end;

function TCustomAuxoDataset.AllocRecordBuffer: TRecordBuffer;
begin
  Result := AllocMem(FRecBufSize);
end;

constructor TCustomAuxoDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FModifiedFields := TList<TField>.Create;
  BookmarkSize := SizeOf(TObject);
  ObjectView := True;
end;

destructor TCustomAuxoDataset.Destroy;
begin
  Close;
  FModifiedFields.Free;
  inherited;
end;

procedure TCustomAuxoDataset.LoadFieldDefsFromFields(AFields: TFields; AFieldDefs: TFieldDefs);
var
  F  : TField;
  FD : TFieldDef;
begin
  for F in AFields do
  begin
    if AFieldDefs.IndexOf(F.FieldName) = -1 then
    begin
      FD          := AFieldDefs.AddFieldDef;
      FD.Name     := F.FieldName;
      FD.DataType := F.DataType;
      FD.Size     := F.Size;
      if F.Required then
        FD.Attributes := [faRequired];
//      if ReadOnly then
//        FD.Attributes := FD.Attributes + [faReadonly];
      if (F.DataType = ftBCD) and (F is TBCDField) then
        FD.Precision := TBCDField(F).Precision;
      if F is TObjectField then
        LoadFieldDefsFromFields(TObjectField(F).Fields, FD.ChildDefs);
    end;
  end;
end;

function TCustomAuxoDataset.MatchField(Field: TField; Value: Variant; Options: TLocateOptions): Boolean;
var
  FieldValue: string;
begin
  case Field.DataType of
    ftString, ftFixedChar, ftWideString, ftFixedWideChar:
      begin
        FieldValue := VarToStr(Field.Value);
        if loPartialKey in Options then
          FieldValue := Copy(FieldValue, 1, Length(Value));
        if loCaseInsensitive in Options then
          Result := SameText(VarToStr(Value), FieldValue)
        else
          Result := SameStr(VarToStr(Value), FieldValue);
      end;
  else
    Result := (Field.Value = Value);
  end;
end;

function TCustomAuxoDataset.MatchRecord(FieldList: TList<TField>; const KeyValues: Variant; Options: TLocateOptions): boolean;
var
  I: integer;
begin
  Result := False;
  for I := 0 to FieldList.Count - 1 do
  begin
    if FieldList.Count = 1 then
      Result := MatchField(FieldList[I], KeyValues, Options)
    else
      Result := MatchField(FieldList[I], KeyValues[I], Options);
    if not Result then
      Break;
  end;
end;

function TCustomAuxoDataset.DoLocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean; ResultFields: string): Boolean;
var
  FieldList: TList<TField>;
  OldIndex: Integer;
  Buffer: TAuxoRecordBuffer;
  ResultFieldList: TList<TField>;
  I: integer;
  NullValueBuffer: TValueBuffer;
begin
  CheckBrowseMode;
  CursorPosChanged;
  Result := False;
  FieldList := TList<TField>.Create;
  try
    GetTheFieldList(FieldList, KeyFields);
    OldIndex := FCurrent;
    SetTempState(dsFilter);
    Buffer := TempBuffer;
    FFilterBuffer := Buffer;
    try
      InternalFirst;
      while GetRecord(Buffer, gmNext, True) = grOK do
        if MatchRecord(FieldList, KeyValues, Options) then
        begin
          Result := True;
          break;
        end;


      if Result and not SyncCursor then
      begin
        // For lookups, read needed field values into the temp buffer
        ResultFieldList := TList<TField>.Create;
        try
          GetTheFieldList(ResultFieldList, ResultFields);
          // For optimization, update only the values for desired result fields
          for I := 0 to ResultFieldList.Count - 1 do
          begin
//            {$IFDEF DELPHIXE4_LVL}
              SetLength(NullValueBuffer, 0);
              GetFieldData(ResultFieldList[I], NullValueBuffer);
//            {$ELSE}
//              {$IFDEF DELPHIXE3_LVL}
//              GetFieldData(ResultFieldList[I], TValueBuffer(nil));
//              {$ELSE}
//              GetFieldData(ResultFieldList[I], nil);
//              {$ENDIF}
//            {$ENDIF}
          end;
//          CalculateFields(Buffer);
        finally
          ResultFieldList.Free;
        end;
      end;

      // Restore old cursor position
      if not (Result and SyncCursor) then
        FCurrent := OldIndex;
    finally
      RestoreState(dsBrowse);
    end;
  finally
    FieldList.Free;
  end;
end;

end.

