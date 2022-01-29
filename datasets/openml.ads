
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON; use GNATCOLL.JSON;

package Openml is

   package ML_Qualities_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, JSON_Value);
   subtype Qualities_Map is ML_Qualities_Package.Map;

   type Shape_Data is record
      Num_Samples     : Natural := 0;
      Features_Length : Natural := 0;
   end record;

   procedure Download_Data_To_Bunch (URL              : String;
                                     File_Name        : String := "";
                                     Sparse, As_Frame : Boolean;
                                     Features_List    : JSON_Array;
                                     Data_Columns     : JSON_Array;
                                     Target_Columns   : JSON_Array;
                                     Shape            : Shape_Data);
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
   function Valid_Data_Column_Names
     (Features_List, Target_Columns : JSON_Array) return JSON_Array;

end Openml;
