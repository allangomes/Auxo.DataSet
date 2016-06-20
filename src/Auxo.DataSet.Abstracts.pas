unit Auxo.DataSet.Abstracts;

interface

uses
  System.Generics.Collections;

const
  IAPIProvider_ID = '{13BDB707-9013-4148-BAEB-1DBC7AF57D09}';
  IAPIProvider_GUID: TGUID = IAPIProvider_ID;

type
  TQuery = class
  private
    FSelect: string;
    FOrder: string;
    FWhere: string;
    FSkip: Integer;
    FTake: Integer;
  published
    property Select: string read FSelect write FSelect;
    property Order: string read FOrder write FOrder;
    property Where: string read FWhere write FWhere;
    property Skip: Integer read FSkip write FSkip;
    property Take: Integer read FTake write FTake;
  end;

  IAPIProvider = interface
  [IAPIProvider_ID]
    function Count(AWhere: string = ''): Integer;
    function Load(AQuery: TQuery): string;
    function Insert(var ARecord: string): Boolean;
    function Update(var ARecord: string): Boolean;
    function Delete(AId: Int64): Boolean;
  end;

implementation

end.
