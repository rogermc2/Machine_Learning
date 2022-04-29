
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

package ML_Types is

   Max_Features : constant Integer := 100;
   type Data_Type is (Integer_Type, Float_Type, Boolean_Type, UB_String_Type);
   pragma Ordered (Data_Type);

   type Class_Range is new Positive range 1 .. Max_Features;
   subtype Feature_Name_Type is Unbounded_String;
   type Feature_Names_Array is array (Class_Range range <>)
     of Feature_Name_Type;
   type Feature_Data_Array is array (Class_Range range <>) of Unbounded_String;
   type Data_Array is array (Class_Range range <>) of Unbounded_String;
   type Features_ID_Array is array (Class_Range range <>) of Positive;
   type Class_Names_Array is array (Class_Range range <>) of Unbounded_String;
   type Data_Type_Array is array (Class_Range range <>) of Data_Type;
   subtype Feature_Class is Class_Range;
   subtype Question_Type is Class_Range;

   package Integer_Package is new Ada.Containers.Vectors (Positive, Integer);
   subtype Integer_List is Integer_Package.Vector;

   use Integer_Package;
   package Integer_Package_2D is new Ada.Containers.Vectors (Positive, Integer_List);
   subtype Integer_List_2D is Integer_Package_2D.Vector;

   package Integer_DLL_Package is new
     Ada.Containers.Doubly_Linked_Lists (Integer);
   subtype Integer_DL_List is Integer_DLL_Package.List;

   package Character_Package is new Ada.Containers.Vectors
     (Positive, Character);
   subtype Character_List is Character_Package.Vector;

   package Unbounded_Package is new Ada.Containers.Vectors
     (Positive, Unbounded_String);
   subtype Unbounded_List is Unbounded_Package.Vector;
   subtype Features_List is Unbounded_Package.Vector;
   subtype Class_Names_List is Unbounded_Package.Vector;
   subtype Feature_Names_List is Unbounded_Package.Vector;

   type Node_Kind is (Undefined_Node, Decision_Node, Prediction_Node);

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

   use Unbounded_Package;
   package Raw_Data_Package is new Ada.Containers.Vectors
     (Positive, Unbounded_List);
   subtype Raw_Data_Vector is Raw_Data_Package.Vector;

   type Data_Rows is array (Integer range <>) of Unbounded_String;

   type Features_Data (Class_Count : Class_Range := 2) is record
      Features   : Features_ID_Array (1 .. Class_Count);
   end record;

   package Label_Type_Package is new Ada.Containers.Ordered_Maps
     (Class_Range, Data_Type);
   subtype Label_Type_Map is Label_Type_Package.Map;

   type Value_Data (Feature_Kind : Data_Type := Integer_Type) is record
      Feature_Name : Feature_Name_Type;
      case Feature_Kind is
         when Integer_Type => Integer_Value     : Integer;
         when Float_Type => Float_Value         : Float;
         when Boolean_Type => Boolean_Value     : Boolean;
         when UB_String_Type => UB_String_Value : Unbounded_String;
      end case;
   end record;

   package Values_Package is new Ada.Containers.Vectors
     (Class_Range, Value_Data);
   subtype Value_List is Values_Package.Vector;

   type Value_Record (Value_Kind : Data_Type := Integer_Type) is record
      case Value_Kind is
         when Integer_Type => Integer_Value     : Integer := 0;
         when Float_Type => Float_Value         : Float := 0.0;
         when Boolean_Type => Boolean_Value     : Boolean := False;
         when UB_String_Type => UB_String_Value : Unbounded_String
              := To_Unbounded_String ("");
      end case;
   end record;

   package Value_Data_Package is new
     Ada.Containers.Vectors (Positive, Value_Record);
   subtype Value_Data_List is Value_Data_Package.Vector;

   function "<" (L, R : Value_Record) return Boolean;
   function "<=" (L, R : Value_Record) return Boolean;

   function "=" (L, R : Value_Record) return Value_Record;
   function "+" (L, R : Value_Record) return Value_Record;
   function "-" (L, R : Value_Record) return Value_Record;
   function "abs" (Value : Value_Record) return Value_Record;

   function "=" (L, R : Value_Data_List) return Value_Data_List;
   function "-" (L, R : Value_Data_List) return Value_Data_List;
   function "abs" (aVector : Value_Data_List) return Value_Data_List;
   procedure Check_Length (Routine_Name : String; L, R : Value_Data_List);
   function Dot (L, R : Value_Data_List) return Value_Record;

   package Value_Data_Sorting is new
     Value_Data_Package.Generic_Sorting ("<");

   use Value_Data_Package;
   package Value_Lists_Data_Package is new
     Ada.Containers.Vectors (Positive, Value_Data_List);
   subtype Value_Data_Lists_2D is Value_Lists_Data_Package.Vector;

   function "=" (L, R : Value_Data_Lists_2D) return Value_Data_Lists_2D;
   function "-" (L, R : Value_Data_Lists_2D) return Value_Data_Lists_2D;
   function "abs" (aVector : Value_Data_Lists_2D) return Value_Data_Lists_2D;
   procedure Check_Lengths (Routine_Name : String; L, R : Value_Data_Lists_2D);
   function Dot (L, R : Value_Data_Lists_2D) return Value_Record;

   use Value_Lists_Data_Package;
   package Value_Lists_3D_Package is new
     Ada.Containers.Vectors (Positive, Value_Data_Lists_2D);
   subtype Value_Data_Lists_3D is Value_Lists_3D_Package.Vector;

   type Value_Data_Array is array (Positive range <>) of Value_Record;
   type Value_Data_Array_2D is array (Positive range <>, Positive range <>)
     of Value_Record;
   type Value_Data_Array_3D is array
     (Positive range <>, Positive range <>, Positive range <>)
     of Value_Record;

   package Count_Package is new Ada.Containers.Ordered_Maps
     (Data_Type, Natural);
   subtype Count_Map is Count_Package.Map;

   package Boolean_Label_Map_Package is new Ada.Containers.Ordered_Maps
     (Boolean, Natural);
   subtype Boolean_Label_Map is Boolean_Label_Map_Package.Map;

   package Float_Label_Map_Package is new Ada.Containers.Ordered_Maps
     (Float, Natural);
   subtype Float_Label_Map is Float_Label_Map_Package.Map;

   package Integer_Label_Map_Package is new Ada.Containers.Ordered_Maps
     (Integer, Natural);
   subtype Integer_Label_Map is Integer_Label_Map_Package.Map;

   package UB_Label_Map_Package is new Ada.Containers.Ordered_Maps
     (Unbounded_String, Natural);
   subtype UB_Label_Map is UB_Label_Map_Package.Map;

   type Label_Maps is record
      Boolean_Map   : Boolean_Label_Map;
      Float_Map     : Float_Label_Map;
      Integer_Map   : Integer_Label_Map;
      UB_String_Map : UB_Label_Map;
   end record;

   subtype Raw_Label is Unbounded_String;

   type Prediction_Data is record
      Label      : Unbounded_String;
      Num_Copies : Natural := 1;
   end record;

   package Prediction_Data_Package is new
     Ada.Containers.Doubly_Linked_Lists (Prediction_Data);
   subtype Predictions_List is Prediction_Data_Package.List;

   package Indefinite_String_Package is new
     Ada.Containers.Indefinite_Doubly_Linked_Lists (String);
   subtype Indef_String_List is Indefinite_String_Package.List;

   package String_Package is new Ada.Containers.Doubly_Linked_Lists
     (Ada.Strings.Unbounded.Unbounded_String);
   subtype String_List is String_Package.List;

   use String_Package;
   package String_List_Package is new Ada.Containers.Doubly_Linked_Lists
     (String_List);
   subtype String_Multi_List is String_List_Package.List;

   package String_Vector_Package is new Ada.Containers.Vectors
     (Positive, Ada.Strings.Unbounded.Unbounded_String);
   subtype String_Vector is String_Vector_Package.Vector;

   use String_Vector_Package;
   package String_Multi_Vector_Package is new Ada.Containers.Vectors
     (Positive, String_Vector);
   subtype String_Multi_Vector is String_Multi_Vector_Package.Vector;

   type Data_Record (Label_Kind : Data_Type := Integer_Type) is record
      Feature_Names  : String_List;
      Label_Name     : Unbounded_String := To_Unbounded_String ("");
      Feature_Values : Value_Data_Lists_2D;
      Label_Values   : Value_Data_Lists_2D;   --  num outputs x num values
   end record;

   type Multi_Output_Data_Record is record
      Feature_Names  : String_List;
      Label_Names    : Unbounded_List;
      Feature_Values : Value_Data_Lists_2D;
      Label_Values   : Value_Data_Lists_2D;   --  num outputs x num values
   end record;

   type Raw_Question is record
      Feature_Name  : Feature_Name_Type;  --  e.g. "Colour"
      Feature_Value : Unbounded_String;  --  e.g. "Green"
   end record;

   type Question_Data (Feature_Kind : Data_Type := Integer_Type) is record
      Feature_Name : Feature_Name_Type := To_Unbounded_String ("");
      Gain         : Float := 0.0;
      case Feature_Kind is
         when Integer_Type => Integer_Value     : Integer := 0;
         when Float_Type => Float_Value         : Float := 0.0;
         when Boolean_Type => Boolean_Value     : Boolean := False;
         when UB_String_Type => UB_String_Value : Unbounded_String :=
                                                       To_Unbounded_String ("");
      end case;
   end record;

   type Tree_Node_Type (Node_Type : Node_Kind := Undefined_Node) is record
      Decision_Branch : Boolean := True;
      case Node_Type is
         when Decision_Node =>
            Question        : Question_Data;
            Gini            : Float := 0.0;
            True_Branch     : Rows_Vector := Rows_Package.Empty_Vector;
            False_Branch    : Rows_Vector := Rows_Package.Empty_Vector;
         when Prediction_Node =>
            Rows            : Rows_Vector := Rows_Package.Empty_Vector;
            Prediction      : Row_Data;
            Prediction_List : Predictions_List;
         when Undefined_Node => null;
      end case;
   end record;

   type Partitioned_Rows is record
      True_Rows  : Rows_Vector;
      False_Rows : Rows_Vector;
   end record;

   package Tree_Package is new Ada.Containers.Indefinite_Multiway_Trees
     (Tree_Node_Type);
   subtype Tree_Type is Tree_Package.Tree;
   subtype Tree_Cursor is Tree_Package.Cursor;

   package Strings_Package is new Ada.Containers.Doubly_Linked_Lists
     (Unbounded_String);
   subtype Strings_List is Strings_Package.List;

end ML_Types;
