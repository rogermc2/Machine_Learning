
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_22A is

   type Float_Data_Array is array (Integer range <>) of Float;
   type Boolean_Data_Array is array (Integer range <>) of Boolean;

   type Row_Record is record
      Treatment  : Boolean;
      Float_Data : Float_Data_Array (2 .. 11);
      X7_25      : Boolean_Data_Array (12 .. 25);
   end record;

   package Data_Package is new
     Ada.Containers.Doubly_Linked_Lists (Row_Record);
   subtype Data_List is Data_Package.List;

   type Data_Record is record
      Col_Names  : ML_Types.Indef_String_List;
      Data       : Data_List;
   end record;

   function Get_Data (File_Name : String) return Data_Record;

end Support_22A;
