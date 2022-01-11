
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON; use GNATCOLL.JSON;

package Openml is

   package ML_Qualities_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, JSON_Value);
   subtype Qualities_Map is ML_Qualities_Package.Map;

   procedure Fetch_Openml (Dataset_Name  : String;  Version : String := "";
                           Data_Id       : in out Integer;
                           Target_Column : String := "default-target";
--                             Return_X_Y    : Boolean := False;
                           As_Frame      : String := "false");
   function Get_Data_Description_By_ID
     (Data_ID : Integer; File_Name : String := "") return JSON_Value;
   function Get_Data_Features (Data_ID   : Integer;
                               File_Name : String := "") return JSON_Array;
   function Get_Data_Info_By_Name (Dataset_Name : String;
                                   Version      : String := "";
                                   Active       : Boolean := False;
                                   File_Name    : String := "")
                                   return JSON_Value;
   function Get_Data_Qualities (Data_ID : Integer; Dataset_Name : String := "")
                                return Qualities_Map;

end Openml;
