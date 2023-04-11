
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;

package Support_12A is

   type Data_Record is record
      Features : Integer_Array_List;
      Labels   : ML_Types.Integer_List;
   end record;

   type Dictionary_Record is record
      Key   : Unbounded_String;
      Value : Natural := 0;
   end record;

   package Dictionary_Package is new
     Ada.Containers.Doubly_Linked_Lists (Dictionary_Record);
   subtype Dictionary_List is Dictionary_Package.List;

   function Get_Data (File_Name : String; Dictionary : Dictionary_List)
                      return Data_Record;
   function Play_Game (Classifier   : Python.Module; Rounds : Positive;
                       Data, Labels : Integer_Array; Alpha : Integer)
                       return Integer;
   function Read_Vocabulary (File_Name : String) return Dictionary_List;
   function To_Integer_Array (A : Integer_Array_List) return Integer_Array;
   pragma Inline (To_Integer_Array);

end Support_12A;
