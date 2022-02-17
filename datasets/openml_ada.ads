
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with GNATCOLL.JSON; use GNATCOLL.JSON;

with ML_Types;

with Load_ARFF_Data;

package Openml_Ada is

   type As_Frame_State is (As_Frame_False, As_Frame_True, As_Frame_Auto);
   subtype Qualities_Map is JSON_Array;

   type Bunch_Data (Only_XY : Boolean := False) is record
      Data          : ML_Types.Value_Data_List;
      Target        : JSON_Array;
      case Only_XY is
         when True => null;
         when False =>
            As_Frame      : As_Frame_State := As_Frame_False;
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
     (ARFF_Container : Load_ARFF_Data.ARFF_Record;
      Sparse         : Boolean;
      As_Frame       : As_Frame_State := As_Frame_False;
      Features_List  : Load_ARFF_Data.Attribute_List;
      Data_Columns, Target_Columns : ML_Types.String_List;
      Return_X_Y     : Boolean := False)
      return Bunch_Data;
   function Fetch_Openml (Dataset_File_Name : String;
                          Target_Column     : ML_Types.String_List;
                          As_Frame          : in out As_Frame_State;
                          Return_X_Y        : Boolean := False)
                           return Bunch_Data;
   function Get_Data_Description_By_ID (Data_ID : Integer) return JSON_Value;
   function Get_Data_Features (Data_ID : Integer) return JSON_Array;
   function Get_Data_Info_By_Name (Dataset_Name : String) return JSON_Value;
   function Get_Data_Qualities (Data_ID : Integer) return Qualities_Map;
   function J_Array_To_String_List (J_Array : JSON_Array)
                                    return ML_Types.String_List;
   function Valid_Data_Column_Names
     (Features_List, Target_Columns : JSON_Array) return ML_Types.String_List;

end Openml_Ada;
