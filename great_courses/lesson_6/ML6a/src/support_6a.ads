
with Ada.Containers.Ordered_Maps;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_6A is

   package Vocablary_Dictionary_Package is new
     Ada.Containers.Ordered_Maps (Unbounded_String, Natural);
   subtype Vocablary_Dictionary_Map is Vocablary_Dictionary_Package.Map;

   type Data_Record is record
      Features : Integer_Array_List;
      Labels   : ML_Types.Integer_List;
   end record;

   function Get_Data (File_Name : String; Dictionary : Vocablary_Dictionary_Map)
                      return Data_Record;
   function Read_Vocabulary (File_Name : String)
                             return Vocablary_Dictionary_Map;
   function Tokenize (Data : String; Dictionary : Vocablary_Dictionary_Map)
                      return Integer_Array;

end Support_6A;
