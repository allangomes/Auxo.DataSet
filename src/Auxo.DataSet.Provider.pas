unit Auxo.DataSet.Provider;

interface

uses
  System.Classes, System.Rtti, Data.DB;

type
  TDataProvider = class(TPersistent)
  public
    function Load: Variant; virtual; abstract;
    function Insert(Value: Variant): Variant; virtual; abstract;
    function Update(Id: Variant; Value: Variant): Variant; virtual; abstract;
    function Delete(Id: Variant): Variant; virtual; abstract;
  end;

  TProviderDataSet = class(TComponent)
  protected
    function FetchMoreRecords: boolean; virtual; abstract;
    function FetchingRecords: boolean; virtual; abstract;
    function NewRecord: TValue; virtual; abstract;
    function GetKey(Rec: TValue): Variant; virtual; abstract;
    function RecordToData(Rec: TValue): Variant; virtual; abstract;
    //function IndexToInsert(ACurrent: TRecord): Integer; virtual; abstract;
    function GetRecord(AIndex: Integer): TValue; virtual; abstract;
    function RecordInsert(ARecord: TValue; AIndex: Integer): Boolean; virtual; abstract;
    function RecordUpdate(ARecord: TValue; ARecordIndex: Integer = 0): Boolean; virtual; abstract;
    function RecordDelete(ARecord: Integer): Boolean; virtual; abstract;

    function RecordUpdateField(Field: TField; Rec: TValue; Value: Variant): Boolean; virtual; abstract;
    function GetFieldValue(Field: TField; Rec: TValue): Variant; virtual; abstract;
    function LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions; SyncCursor: Boolean; ResultFields: string): Boolean; virtual; abstract;
    function ListCount: integer; virtual; abstract;
  end;

  TJsonProviderDataSet = class(TProviderDataSet)
  private
    FJSON: string;
    FList: ISuperArray;
    FDataProvider: TDataProvider;
    procedure SetJSON(const Value: string);
  protected
    DataLoaded: Boolean;
    function FetchMoreRecords: boolean; override;
    function FetchingRecords: boolean; override;
    function NewRecord: TValue; override;
    function RecordToData(Rec: TValue): Variant; override;
    function GetRecord(AIndex: Integer): TValue; override;
    function RecordInsert(ARecord: TValue; AIndex: Integer): Boolean; override;
    function RecordDelete(ARecord: Integer): Boolean; override;
    function RecordUpdate(ARecord: TValue; ACurrent: Integer): Boolean; override;
    function RecordUpdateField(Field: TField; ARecord: TValue; Value: Variant): Boolean; override;
    function GetFieldValue(Field: TField; ARecord: TValue): Variant; override;
    function ListCount: integer; override;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property JSON: string read FJSON write SetJSON;
    property DataProvider: TDataProvider read FDataProvider write FDataProvider;
  end;


implementation

end.
