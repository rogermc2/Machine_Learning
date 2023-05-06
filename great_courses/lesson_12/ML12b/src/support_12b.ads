
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;
with Python;

package Support_12B is

   type Data_Items (Rows : Positive) is record
      Features : Integer_Array_List;
      Labels   : Integer_Matrix (1 .. Rows, 1 .. 1);
   end record;

   type Dictionary_Record is record
      Key   : Unbounded_String;
      Value : Natural := 0;
   end record;

   package Dictionary_Package is new
     Ada.Containers.Doubly_Linked_Lists (Dictionary_Record);
   subtype Dictionary_List is Dictionary_Package.List;

   function ProbA_Chooser
     (Classifier       : Python.Module;
      Current_Item     : Positive; Num_Items : Positive;
      Labeled_Examples : Data_Items;
      Train_Set        : ML_Types.Integer_List_2D;
      Train_Labels     : ML_Types.Integer_List;
      Alpha            : Float) return Integer;
   function Get_Data (File_Name : String; Dictionary : Dictionary_List)
                      return Data_Items;
   function Play_Game (Classifier : Python.Module;
                       Rounds     : Positive; Labeled_Examples : Data_Items;
                       Alpha      : Float) return Natural;
   function Read_Vocabulary (File_Name : String) return Dictionary_List;

end Support_12B;
