
with GNATCOLL.JSON; use GNATCOLL.JSON;

package Openml is

   procedure Fetch_Openml (Dataset_Name  : String;  Version : String := "";
                           Data_Id       : in out Integer;
                           Target_Column : String := "default-target";
                           Return_X_Y    : Boolean := False;
                           As_Frame      : String := "false");
   function Get_Data_Description_By_ID
      (Data_ID : Integer; From_File : Boolean := False) return JSON_Value;
   function Get_Data_Info_By_Name (Dataset_Name : String;
                                   Version : String := "";
                                   Active, From_File : Boolean := False)
                                    return JSON_Value;

end Openml;
