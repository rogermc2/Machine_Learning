
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_6A is

   type Data_Record is record
      Features : Integer_Array_List;
      Labels   : ML_Types.Integer_List;
   end record;

   function Get_Data (File_Name : String; Dictionary : ML_Types.String_Map)
                      return Data_Record;
   function Read_Vocabulary (File_Name : String) return ML_Types.String_Map;
   function Tokenize (Data : String; Dictionary : ML_Types.String_Map)
                      return Integer_Array;
   function Word_List  (Dictionary : ML_Types.String_Map)
                        return ML_Types.Indef_String_List;

end Support_6A;