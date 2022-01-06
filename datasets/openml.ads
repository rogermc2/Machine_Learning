
package Openml is

   procedure Fetch_Openml (Dataset_Name  : String;  Version : Integer;
                           Data_Id       : in out Integer;
                           Target_Column : String := "default-target";
                           Return_X_Y    : Boolean := False;
                           As_Frame      : String := "false");

end Openml;
