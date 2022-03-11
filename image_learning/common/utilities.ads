
with Ada.Containers.Vectors;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Builder;
with IL_Types; use IL_Types;

with Classifier_Utilities;

package Utilities is

    type Integer_Zip_Item is record
        Integer_1 : Integer;
        Integer_2 : Integer;
    end record;

    package Integer_Zip_Package is new
      Ada.Containers.Vectors (Positive, Integer_Zip_Item);
    subtype Integer_Zip_List is Integer_Zip_Package.Vector;

    Utilities_Exception : exception;

    procedure Check_Rows (Rows : in out Rows_Vector);
    function Feature_Array (Data    : Rows_Vector;
                            Col_Num : Class_Range)
                           return Value_Data_Array;
    function Get_Column (List_2D      : Value_Data_Lists_2D;
                         Column_Index : Positive)
                        return Value_Data_List;
    function Get_Data_Type (Data : Unbounded_String) return Data_Type;
    function Is_Boolean (Item : Unbounded_String) return Boolean;
    function Is_Float (Item : Unbounded_String) return Boolean;
    function Is_Integer (Item : Unbounded_String) return Boolean;
    function Label_Array (Data : Rows_Vector)
                         return  Value_Data_Array;
    procedure Load_CSV_Data (Data_File : File_Type;
                             Data      : out Rows_Vector);
    function Load_CSV_Data
      (Data_File : File_Type; Header_Line : out Header_Data_Type)
      return Rows_Vector;
    function Load_Raw_CSV_Data (Data_File : File_Type)
                               return Raw_Data_Vector;
    function Number_Of_Features (Rows : Rows_Vector)
                                return Class_Range;
    function Number_Of_Features (Rows : Value_Data_List)
                                return Class_Range;
    procedure Permute (aList : in out Integer_List);
    pragma Inline (Permute);
    procedure Permute (aList : in out String_List);
    pragma Inline (Permute);
    function Permute (aList : Float_List_2D)
                     return Float_List_2D;
    pragma Inline (Permute);
    function Permute (aList : Float_List_2D)
                     return Float_List_3D;
    pragma Inline (Permute);
    function Predictions (Node : Tree_Node_Type)
                         return Predictions_List;
    function Prediction_String (Label_Counts : Predictions_List)
                               return String;
    procedure Print_Best (Message : String; Best_Split : Builder.Best_Data);
    procedure Print_Classification (Classification : Predictions_List);
    procedure Print_Feature_Values (Message : String; Rows : Rows_Vector;
                                    Column  : Class_Range);
    procedure Print_Feature_Types
      (Message : String; theTypes : Classifier_Utilities.Feature_Type_Array);
    procedure Print_Label_Types
      (Message : String; theTypes : Classifier_Utilities.Label_Type_Array);
    procedure Print_Leaf (Label_Counts : Predictions_List);
    procedure Print_Node (Node : Tree_Node_Type);
    procedure Print_Question (Message  : String;
                              Question : Question_Data);
    procedure Print_Raw_Question
      (Message : String; Question : Raw_Question);
    procedure Print_Row (Message : String; aRow : Row_Data);
    procedure Print_Row (Message : String; Rows : Rows_Vector;
                         Row_Number : Positive);
    procedure Print_Rows (Message : String; Rows : Rows_Vector);
    procedure Print_Tree (aTree : Tree_Package.Tree);
    procedure Print_UB_Label_Counts (Rows : Rows_Vector);
    procedure Print_Unique_Values (Rows    : Rows_Vector;
                                   Feature : Feature_Name_Type);
    procedure Print_Value_Record (Message : String;
                                  Value   : Value_Record);
    function Split_Row_Data (Row_Data : Rows_Vector)
                            return Data_Record;
    function Split_String (aString, Pattern : String)
                          return String_List;
    procedure Swap (Data : in out Float_List_2D; L, R : Positive);
    pragma Inline (Swap);
    procedure Swap (Data : in out Integer_List; L, R : Positive);
    pragma Inline (Swap);
    function XY_To_Rows (X, Y : Value_Data_Lists_2D)
                        return Rows_Vector;
    function Zip (a, b : Integer_List) return Integer_Zip_List;

end Utilities;
