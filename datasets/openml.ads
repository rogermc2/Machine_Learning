
package Openml is

    procedure Fetch_Openml (Dataset_Name : String;  Version : Integer;
                            Data_Id : in out Integer;
                            Return_X_Y : Boolean := False);

end Openml;
