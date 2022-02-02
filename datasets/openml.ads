
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with ML_Types;

package Openml is

   subtype Qualities_Map is JSON_Array;

   type Bunch_Data (Only_XY : Boolean := False) is record
      Data          : JSON_Array;
      Target        : JSON_Array;
      case Only_XY is
         when True => null;
         when False =>
            As_Frame      : Boolean := False;
            Categories    : JSON_Array;
            Feature_Names : JSON_Array;
            Target_Names  : JSON_Array;
      end case;
   end record;

   type Shape_Data is record
      Num_Samples     : Natural := 0;
      Features_Length : Natural := 0;
   end record;

   function Download_Data_To_Bunch
     (URL              : String; File_Name : String := "";
      Sparse, As_Frame : Boolean;  Features_List  : JSON_Array;
      Data_Columns     : JSON_Array; Target_Columns : JSON_Array)
       return Bunch_Data;
   function Fetch_Openml (Dataset_Name  : String;  Version : String := "";
                          File_Name     : String := "";
                          Features_File_Name : String := "";
                          Data_Id       : in out Integer;
                          Target_Column : String := "default-target";
                          Return_X_Y    : Boolean := False;
                          As_Frame      : in out Unbounded_String)
                           return Bunch_Data;
   function Get_Data_Description_By_ID
     (Data_ID : Integer; File_Name : String := "") return JSON_Value;
   function Get_Data_Features (Data_ID   : Integer;
                               File_Name : String := "") return JSON_Array;
   function Get_Data_Info_By_Name (Dataset_Name : String;
                                   Version      : String := "";
                                   Active       : Boolean := False;
                                   File_Name    : String := "")
                                    return JSON_Value;
   function Get_Data_Qualities (Data_ID : Integer; File_Name : String := "")
                                return Qualities_Map;
   function J_Array_To_String_List (J_Array : JSON_Array)
                                    return ML_Types.String_List;
   function Valid_Data_Column_Names
     (Features_List, Target_Columns : JSON_Array) return JSON_Array;

end Openml;
