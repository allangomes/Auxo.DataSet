# Generic DataSet 0.0.1-pre-alpha

* Data Types
  * JSON
  * XML
  * others

* Data Providers
  * RestAPI
  * Databases
  * Local File

* Features
  * Where Suport
  * Pagination Suport
  * Order Suport
  * Master Detail Suport

```delphi
var
  dataset: TAuxoDataSet;
  api: TApiRepository;
begin
  dataset.Repository := api;
  dataset.Order['Name'];
  dataset.Where['Age', TOperator.LtOrEq, 12].Orr['Age', TOperator.GrOtEq, 65];
  dataset.PageCount := 50;
  
  dataset.OpenAsync(procedure
  begin
    ShowMessage(dataset.RecordCount);
    ShowMessage(datasetName.S); //Function S is a Helper Method that call AsString
  end); //http://api/person/?where=age<=12 OR age>=65&order=Name&Select=Name,Age&Skip=1&Take&50
end;
```


  
