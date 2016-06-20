unit Auxo.DataSet;

interface

uses
  Auxo.DataSet.Custom, Data.DB, System.Rtti, System.Classes, System.SysUtils, System.Threading, Auxo.Data.Core, Auxo.Core.PleaseWait, Auxo.Core.Observer, Auxo.Access.Core, System.Generics.Collections,
  Auxo.Query;

type
  TAuxoDataSet = class(TCustomAuxoDataSet)
  private
    FProcOpened: TProc;
    FInternalQuery: QueryAccess;
    FWorking: Boolean;
    FPageCount: Integer;
    FQuery: IQuery;
    FTotalRecords: Integer;
    FOnStartingWork: TNotifyEvent;
    FOnFinishedWork: TNotifyEvent;
    FPleaseWait: TPleaseWait;
    FMaster: TAuxoDataSet;
    FMasterField: string;
    FChildren: TList<TAuxoDataSet>;
    FRepository: IRepository;
    FOpened: Boolean;
    FList: IRecordList;
    FMasterMode: TMasterMode;
    procedure SetOnFinishedWork(const Value: TNotifyEvent);
    procedure SetOnStartingWork(const Value: TNotifyEvent);
    procedure SetWorking(const Value: Boolean);
    function GetValue(Item: string): Variant;
    procedure SetValue(Item: string; const Value: Variant);
    procedure SetMaster(const Value: TAuxoDataSet);
    procedure CloseChildren;
    function ReloadParameters: TRepositoryParameters;
  protected
    function ThreadProtection<T>(Proc: TFunc<T>): T; overload;

    function CreateWait: TPleaseWait; virtual;
    procedure DoStartingWork; virtual;
    procedure DoFinishedWork; virtual;

    function GetQuery: IQuery;
    function DoFetchMoreRecords: Boolean; override;
    function DoFetchingRecords: boolean; override;
    function DoNewRecord: TValue; override;
    function DoRecordToData(Rec: TValue): Variant; override;
    function DoGetRecord(AIndex: Integer): TValue; override;
    function DoRecordInsert(ARecord: TValue; AIndex: Integer): Boolean; override;
    function DoRecordUpdate(ARecord: TValue; ARecordIndex: Integer = 0): Boolean; override;
    function DoRecordDelete(ARecord: Integer): Boolean; override;
    function DoRecordUpdateField(Field: TField; ARecord: TValue; Value: Variant): Boolean; override;
    function DoGetFieldValue(Field: TField; ARecord: TValue): Variant; override;
    function DoListCount: integer; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure DoAfterInsert; override;
    procedure DoAfterOpen; override;
    procedure DoAfterEdit; override;
  public
    procedure OpenAsync(Proc: TProc);
    function Where: WhereAccess;
    function Order: OrderAccess;
    procedure From(AOrigin: string);
    function MoveBy(Distance: Integer): Integer; override;
    property Value[Item: string]: Variant read GetValue write SetValue; default;
    property Working: Boolean read FWorking write SetWorking;
    function KeyValue: Variant;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property AfterCancel;
    property AfterClose;
    property AfterDelete;
    property AfterEdit;
    property AfterInsert;
    property AfterOpen;
    property AfterPost;
    property AfterRefresh;
    property AfterScroll;
    property BeforeCancel;
    property BeforeClose;
    property BeforeDelete;
    property BeforeEdit;
    property BeforeInsert;
    property BeforeOpen;
    property BeforePost;
    property BeforeRefresh;
    property BeforeScroll;

    property Active;
    property KeyField;
    property Master: TAuxoDataSet read FMaster write SetMaster;
    property MasterMode: TMasterMode read FMasterMode write FMasterMode;
    property MasterField: string read FMasterField write FMasterField;
    property PageCount: Integer read FPageCount write FPageCount;
    property OnStartingWork: TNotifyEvent read FOnStartingWork write SetOnStartingWork;
    property OnFinishedWork: TNotifyEvent read FOnFinishedWork write SetOnFinishedWork;
//    property AfterOpen: TDataSetNotifyEvent read FAfterOpen write FAfterOpen;

    property Repository: IRepository read FRepository write FRepository;
  end;

implementation

uses
  System.TypInfo, System.StrUtils, System.Variants;

{ TAuxoDataSet }

procedure TAuxoDataSet.AfterConstruction;
begin
  inherited;
  FChildren := TList<TAuxoDataSet>.Create;
  FQuery := TQuery.Create;
  FInternalQuery := QueryAccess(FQuery);
end;

procedure TAuxoDataSet.BeforeDestruction;
begin
  inherited;
  FList := nil;
  FQuery := nil;
  FChildren.Free;
  if Assigned(FMaster) then
    FMaster.FChildren.Remove(Self);
end;

procedure TAuxoDataSet.CloseChildren;
var
  Child: TDataSet;
begin
  for Child in FChildren do
    Child.Close;
end;

function TAuxoDataSet.CreateWait: TPleaseWait;
begin
  Result := TPleaseWait.Create;
end;

procedure TAuxoDataSet.DoAfterEdit;
var
  child: TAuxoDataSet;
begin
  inherited;
  for child in FChildren do
    child.Open;
end;

procedure TAuxoDataSet.DoAfterInsert;
var
  child: TAuxoDataSet;
begin
  inherited;
  if Assigned(Master) and (MasterMode = FilterRecords) then
    Value[MasterField] := Master.KeyValue;

  for child in FChildren do
    child.Open;
end;

procedure TAuxoDataSet.DoAfterOpen;
begin
  if FOpened then
  begin
    if Assigned(AfterOpen) then
      AfterOpen(Self);
    if not IsEmpty then
      DoAfterScroll;
    if Assigned(FProcOpened) then
      FProcOpened;
  end;
end;

function TAuxoDataSet.DoFetchingRecords: boolean;
begin
  Result := (State = dsInactive) or ((DoListCount < FTotalRecords) and not FWorking);
end;

function TAuxoDataSet.DoFetchMoreRecords: boolean;
begin
  if not DoFetchingRecords then
    Exit(False);

  FRepository.Load(ReloadParameters,
  procedure (Recs: IRecordList)
  var
    Rec: IRecord;
  begin
    if not Assigned(FList) then
    begin
      FList := Recs;
      FOpened := True;
      DoAfterOpen;
    end
    else
    begin
      for Rec in Recs do
        FList.Add(Rec);
    end;
  end);
  Result := True;
end;

procedure TAuxoDataSet.DoFinishedWork;
begin
  if Assigned(FOnFinishedWork) then
    FOnFinishedWork(Self)
  else
  begin
    FPleaseWait.Close;
    FPleaseWait.Free;
  end;
end;

function TAuxoDataSet.DoGetFieldValue(Field: TField; ARecord: TValue): Variant;
var
  rec: IRecord;
begin
  if ARecord.TryAsType<IRecord>(rec) and Assigned(rec) then
  begin
    if (rec[Field.FieldName] = Null) then
      Exit(Null);
    case Field.DataType of
      ftWideString, ftString: Result := rec.S[Field.FieldName];
      ftWord, ftSmallint, ftInteger, ftLargeint: Result := rec.I[Field.FieldName];
      ftBoolean: Result := rec.D[Field.FieldName];
      ftBCD, ftCurrency, ftFloat: Result := rec.F[Field.FieldName];
      ftDateTime, ftTime, ftDate: Result := rec.D[Field.FieldName];
      ftGuid: Result := rec.S[Field.FieldName];
    end
  end
  else
    Result := Null;
end;

function TAuxoDataSet.DoGetRecord(AIndex: Integer): TValue;
begin
  if FList.Count > 0 then
    Result := TValue.From(FList.O[AIndex])
  else
    Result := TValue.Empty;
end;

function TAuxoDataSet.DoListCount: integer;
begin
  if Assigned(FList) then
    Result := FList.Count
  else
    Result := -1;
end;

function TAuxoDataSet.DoNewRecord: TValue;
begin
  Result := TValue.From(FRepository.New(ReloadParameters));
end;

function TAuxoDataSet.DoRecordDelete(ARecord: Integer): Boolean;
begin
  Result := FRepository.Delete(ReloadParameters);
  if Result then
  begin
    FList.Delete(ARecord);
    Dec(FTotalRecords);
  end;
end;

function TAuxoDataSet.DoRecordInsert(ARecord: TValue; AIndex: Integer): Boolean;
var
  Rec: IRecord;
  child: TAuxoDataSet;
begin
  if Assigned(Master) and (MasterMode = FilterRecords) then
    Value[MasterField] := Master.KeyValue;
  Rec := ARecord.AsType<IRecord>;

  for child in FChildren do
    Rec.L[child.MasterField] := child.FList;
  Result := FRepository.Insert(ReloadParameters, Rec);
  if Result then
  begin
    FList.Add(Rec);
    Inc(FTotalRecords);
  end
end;

function TAuxoDataSet.DoRecordToData(Rec: TValue): Variant;
begin
  Result := IntPtr(Rec.AsType<IRecord>);
end;

function TAuxoDataSet.DoRecordUpdate(ARecord: TValue; ARecordIndex: Integer): Boolean;
var
  Rec: IRecord;
  child: TAuxoDataSet;
begin
  Rec := ARecord.AsType<IRecord>;

  for child in FChildren do
    Rec.L[child.MasterField] := child.FList;
  Result := FRepository.Update(ReloadParameters, Rec);
  if Result then
    FList[ARecordIndex] := Rec;
end;

function TAuxoDataSet.DoRecordUpdateField(Field: TField; ARecord: TValue; Value: Variant): Boolean;
begin
  ARecord.AsType<IRecord>[Field.FieldName] := Value;
  Result := True;
end;

procedure TAuxoDataSet.DoStartingWork;
begin
  if Assigned(FOnStartingWork) then
    FOnStartingWork(Self)
  else
  begin
    FPleaseWait := CreateWait;
    FPleaseWait.Show;
  end;
end;

procedure TAuxoDataSet.From(AOrigin: string);
begin
  FQuery.Compile.From := AOrigin;
end;

function TAuxoDataSet.GetQuery: IQuery;
begin
  if FInternalQuery.Order.Count = 0 then
    FInternalQuery.Order['LastUpdate', TOrder.Desc];
  if PageCount > 0 then
    FInternalQuery.SkipTake(DoListCount, PageCount);
  if Assigned(Master) and (MasterMode = FilterRecords) then
    FInternalQuery.Where[MasterField, Master.KeyValue];
  Result := FQuery;
end;

function TAuxoDataSet.GetValue(Item: string): Variant;
begin
  Result := FieldByName(Item).Value;
end;

procedure TAuxoDataSet.InternalClose;
begin
  inherited;
  FList := nil;
  CloseChildren;
end;

procedure TAuxoDataSet.InternalOpen;
var
  Field: TField;
begin
  FRepository.Count(ReloadParameters,
  procedure (count: Integer)
  begin
    FTotalRecords := count;
  end);
  FInternalQuery.ClearSelect;
  for Field in Fields do
    FInternalQuery.Select(Field.FieldName);
  inherited;
end;

function TAuxoDataSet.KeyValue: Variant;
begin
  if not ContainsText(KeyField, ';') then
    Result := FieldByName(KeyField).Value;
end;

function TAuxoDataSet.MoveBy(Distance: Integer): Integer;
begin
  Result := inherited MoveBy(Distance);
  CloseChildren;
end;

procedure TAuxoDataSet.OpenAsync(Proc: TProc);
begin
  FProcOpened := Proc;
  Open;
end;

function TAuxoDataSet.Order: OrderAccess;
begin
  Result := FInternalQuery.Order;
end;

function TAuxoDataSet.ReloadParameters: TRepositoryParameters;
begin
  Result.Query := GetQuery;
  Result.Source := FQuery.Compile.From;
  Result.KeyField := KeyField;
  Result.KeyValue := KeyValue;
  if Assigned(Master) then
  begin
    Result.Master.KeyField := MasterField;
    Result.Master.KeyValue := Master.KeyValue;
    Result.Master.MasterMode := MasterMode;
    if Result.Master.MasterMode = SourceMaster then
      Result.Source := Master.FQuery.Compile.From;
  end;
  Result.AsyncMethods := [];
end;

procedure TAuxoDataSet.SetMaster(const Value: TAuxoDataSet);
begin
  if Assigned(FMaster) and FMaster.FChildren.Contains(Self) then
    FMaster.FChildren.Remove(Self);
  FMaster := Value;
  if not FMaster.FChildren.Contains(Self) then
    FMaster.FChildren.Add(Self);
end;

procedure TAuxoDataSet.SetOnFinishedWork(const Value: TNotifyEvent);
begin
  FOnFinishedWork := Value;
end;

procedure TAuxoDataSet.SetOnStartingWork(const Value: TNotifyEvent);
begin
  FOnStartingWork := Value;
end;

procedure TAuxoDataSet.SetValue(Item: string; const Value: Variant);
begin
  FieldByName(Item).Value := Value;
end;

procedure TAuxoDataSet.SetWorking(const Value: Boolean);
begin
  if (FWorking <> Value) then
  begin
    FWorking := Value;
    if Value then
    begin
      DisableControls;
      DoStartingWork
    end
    else
    begin
      TThread.Synchronize(TThread.CurrentThread,
      procedure
      begin
        DoFinishedWork;
        SetState(TDatasetState.dsBrowse);
        Refresh;
        EnableControls;
        FProcOpened;
      end);
    end;
  end;
end;

function TAuxoDataSet.ThreadProtection<T>(Proc: TFunc<T>): T;
var
  future: IFuture<T>;
begin
  future := TTask.Future<T>(
  function: T
  begin
    Result := Proc();
    Working := False;
  end).Start;
  Working := True;
  SetState(TDatasetState.dsOpening);
end;

function TAuxoDataSet.Where: WhereAccess;
begin
  Result := FInternalQuery.Where('USER');
end;

end.
