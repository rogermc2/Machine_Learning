
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ML_Types is

   Max_Features : constant Integer := 100;
   type Data_Type is (Integer_Type, Float_Type, Boolean_Type, UB_String_Type);
   pragma Ordered (Data_Type);

   type Class_Range is new Positive range 1 .. Max_Features;
   type Feature_Name_Type is new Unbounded_String;
   type Feature_Names_Array is array (Class_Range range <>) of Feature_Name_Type;
   type Feature_Data_Array is array (Class_Range range <>) of Unbounded_String;
   type Features_ID_Array is array (Class_Range range <>) of Positive;
   type Data_Type_Array is array (Class_Range range <>) of Data_Type;
   subtype Feature_Class is Class_Range;
   subtype Question_Type is Class_Range;

   type Node_Kind is (Decision_Kind, Prediction_Kind);

   type Header_Data_Type (Class_Count : Class_Range := 2) is record
      Features      : Feature_Data_Array (1 .. Class_Count);
      Feature_Types : Data_Type_Array (1 .. Class_Count);
      Label         : Unbounded_String;
   end record;

   type Row_Data (Class_Count : Class_Range := 2) is record
      Features : Feature_Data_Array (1 .. Class_Count);
      Label    : Unbounded_String;
   end record;

   package Rows_Package is new Ada.Containers.Vectors (Positive, Row_Data);
   subtype Rows_Vector is Rows_Package.Vector;

   type Data_Rows is array (Integer range <>) of Unbounded_String;

   type Features_Data (Class_Count : Class_Range := 2) is record
      Features   : Features_ID_Array (1 .. Class_Count);
   end record;

   package Label_Type_Package is new Ada.Containers.Ordered_Maps
     (Class_Range, Data_Type);
   subtype Label_Type_Map is Label_Type_Package.Map;

   type Label_Data (Label_Kind : Data_Type := Integer_Type) is record
      case Label_Kind is
         when Integer_Type => Integer_Value : Integer;
         when Float_Type => Float_Value   : Float;
         when Boolean_Type => Boolean_Value : Boolean;
         when UB_String_Type => UB_String_Value : Unbounded_String;
      end case;
   end record;

   package Count_Package is new Ada.Containers.Ordered_Maps
     (Data_Type, Natural);

   package UB_Label_Map_Package is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Natural);
   subtype UB_Label_Map is UB_Label_Map_Package.Map;

   package Integer_Label_Map_Package is new Ada.Containers.Ordered_Maps
     (Integer, Natural);
   subtype Integer_Label_Map is Integer_Label_Map_Package.Map;

   subtype Raw_Label is Unbounded_String;

   type Raw_Question is record
      Feature_Name  : Feature_Name_Type;  --  e.g. "Colour"
      Feature_Value : Unbounded_String;  --  e.g. "Green"
   end record;

   type Question_Data (Feature_Kind : Data_Type := Integer_Type) is record
      Feature_Name : Feature_Name_Type := To_Unbounded_String ("");
      Gain         : Float := 0.0;
      case Feature_Kind is
         when Integer_Type => Integer_Value : Integer := 0;
         when Float_Type => Float_Value : Float := 0.0;
         when Boolean_Type => Boolean_Value : Boolean := False;
         when UB_String_Type => UB_String_Value : Unbounded_String :=
            To_Unbounded_String ("");
      end case;
   end record;

   type Tree_Node_Type (Node_Type : Node_Kind := Decision_Kind) is record
      Rows     : Rows_Vector := Rows_Package.Empty_Vector;
      Question : Question_Data;
         Branch     : Boolean := True;
      case Node_Type is
      when Decision_Kind => null;
      when Prediction_Kind =>
         Prediction : Row_Data;
      end case;
   end record;

   type Partitioned_Rows is record
      True_Rows  : Rows_Vector;
      False_Rows : Rows_Vector;
   end record;

    type Prediction_Data is record
        Label      : Unbounded_String;
        Num_Copies : Natural := 1;
    end record;

    package Prediction_Data_Package is new
      Ada.Containers.Doubly_Linked_Lists (Prediction_Data);
    subtype Prediction_Data_List is Prediction_Data_Package.List;

   package Tree_Package is new Ada.Containers.Indefinite_Multiway_Trees
     (Tree_Node_Type);
   subtype Tree_Type is Tree_Package.Tree;
   subtype Tree_Cursor is Tree_Package.Cursor;

   package Strings_Package is new Ada.Containers.Doubly_Linked_Lists
     (Unbounded_String);

end ML_Types;
