
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with NL_Types; use NL_Types;

with Classifier_Utilities;

package Utilities is

   type Integer_Pair is record
      Integer_1 : Integer;
      Integer_2 : Integer;
   end record;

   package Integer_Pair_Package is new
     Ada.Containers.Vectors (Positive, Integer_Pair);
   subtype Integer_Pair_List is Integer_Pair_Package.Vector;

   type Float_Pair is record
      Float_1 : Float;
      Float_2 : Float;
   end record;

   package Float_Pair_Package is new
     Ada.Containers.Vectors (Positive, Float_Pair);
   subtype Float_Pair_List is Float_Pair_Package.Vector;

   Utilities_Exception : exception;

   procedure Check_Rows (Rows : in out Rows_Vector);
   function Get_Column (List_2D      : Value_Data_Lists_2D;
                        Column_Index : Positive)
                         return Value_Data_List;
   function Get_Data_Type (Data : Unbounded_String) return Data_Type;
   function Is_Boolean (Item : Unbounded_String) return Boolean;
   function Is_Float (Item : Unbounded_String) return Boolean;
   function Is_Integer (Item : Unbounded_String) return Boolean;
   function Load_Raw_CSV_Data (Data_File : File_Type)
                                return Raw_Data_Vector;
   function Number_Of_Features (Rows : Rows_Vector)
                                 return Class_Range;
   function Number_Of_Features (Rows : Value_Data_List)
                                 return Class_Range;
--     function Pair_Items (A, B : Float_Array) return Float_Pair_List;
   function Pair_Items (A, B : Integer_Array) return Integer_Pair_List;
   procedure Permute (aList : in out Integer_Array);
   pragma Inline (Permute);
   procedure Permute (aList : in out String_List);
   pragma Inline (Permute);
--     function Permute (aList : Float_Matrix) return Float_Matrix;
--     pragma Inline (Permute);
   function Permute (aList : Real_Float_Matrix) return Real_Float_Matrix;
   pragma Inline (Permute);
   procedure Print_Feature_Values (Message : String; Rows : Rows_Vector;
                                   Column  : Class_Range);
   procedure Print_Feature_Types
     (Message : String; theTypes : Classifier_Utilities.Feature_Type_Array);
   procedure Print_Float_Pairs (Message : String; Pairs : Float_Pair_List);
   procedure Print_Integer_Pairs (Message : String; Pairs : Integer_Pair_List);
   procedure Print_Label_Types
     (Message : String; theTypes : Classifier_Utilities.Label_Type_Array);
   procedure Print_Row (Message : String; aRow : Row_Data);
   procedure Print_Row (Message    : String; Rows : Rows_Vector;
                        Row_Number : Positive);
   procedure Print_Rows (Message : String; Rows : Rows_Vector);
   procedure Print_Unique_Values (Rows    : Rows_Vector;
                                  Feature : Feature_Name_Type);
   procedure Print_Value_Record (Message : String;
                                 Value   : Value_Record);
   function Split_String (aString, Pattern : String)
                           return String_List;
   procedure Swap (Data : in out Real_Float_Matrix; L, R : Positive);
   pragma Inline (Swap);
   procedure Swap (Data : in out Integer_Array; L, R : Positive);
   pragma Inline (Swap);
   function XY_To_Rows (X, Y : Value_Data_Lists_2D)
                         return Rows_Vector;

end Utilities;
