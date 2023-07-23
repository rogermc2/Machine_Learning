
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

--  with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Support_Do_Why is

   type Float_Data_Array is array (Integer range <>) of Float;
   type Boolean_Data_Array is array (Integer range <>) of Boolean;

   type Row_Record is record
      Treatment  : Boolean;
      Float_Data : Float_Data_Array (2 .. 11);
      X7_25      : Boolean_Data_Array (12 .. 30);
   end record;

   package Data_Package is new
     Ada.Containers.Doubly_Linked_Lists (Row_Record);
   subtype Data_List is Data_Package.List;

   type Data_Record is record
      Col_Names  : ML_Types.Indef_String_List;
      Data       : Data_List;
   end record;

   function Get_Data (File_Name : String) return Data_Record;
   function Get_X_Names (Names : ML_Types.Indef_String_List)
                         return Unbounded_String;
   procedure Print_Data (theList : Data_Record; Start : Positive := 1;
                         Finish  : Natural := 0);

end Support_Do_Why;
