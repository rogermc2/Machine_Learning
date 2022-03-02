
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Text_IO; use Ada.Text_IO;

with Builder;
with IL_Types;

with Classifier_Utilities;

package Utilities is

   Utilities_Exception : exception;

   procedure Check_Rows (Rows : in out IL_Types.Rows_Vector);
   function Feature_Array (Data    : IL_Types.Rows_Vector;
                           Col_Num : IL_Types.Class_Range)
                            return IL_Types.Value_Data_Array;
   function Get_Column (List_2D      : IL_Types.Value_Data_Lists_2D;
                        Column_Index : Positive)
                         return IL_Types.Value_Data_List;
   function Get_Data_Type (Data : Unbounded_String) return IL_Types.Data_Type;
   function Is_Boolean (Item : Unbounded_String) return Boolean;
   function Is_Float (Item : Unbounded_String) return Boolean;
   function Is_Integer (Item : Unbounded_String) return Boolean;
   function Label_Array (Data : IL_Types.Rows_Vector)
                          return  IL_Types.Value_Data_Array;
   procedure Load_CSV_Data (Data_File : File_Type;
                            Data      : out IL_Types.Rows_Vector);
   function Load_CSV_Data
     (Data_File : File_Type; Header_Line : out IL_Types.Header_Data_Type)
       return IL_Types.Rows_Vector;
   function Load_Raw_CSV_Data (Data_File : File_Type)
                               return IL_Types.Raw_Data_Vector;
   function Number_Of_Features (Rows : IL_Types.Rows_Vector)
                                 return IL_Types.Class_Range;
   function Number_Of_Features (Rows : IL_Types.Value_Data_List)
                                 return IL_Types.Class_Range;
   procedure Permute (aList : in out IL_Types.Integer_List);
   procedure Permute (aList : in out IL_Types.String_List);
   function Permute (aList : IL_Types.Value_Data_Lists_2D)
                     return IL_Types.Value_Data_Lists_2D;
   pragma Inline (Permute);
   function Permute (aList : IL_Types.Value_Data_Lists_2D)
                     return IL_Types.Value_Data_Lists_3D;
   function Predictions (Node : IL_Types.Tree_Node_Type)
                          return IL_Types.Predictions_List;
   function Prediction_String (Label_Counts : IL_Types.Predictions_List)
                                return String;
   procedure Print_Best (Message : String; Best_Split : Builder.Best_Data);
   procedure Print_Classification (Classification : IL_Types.Predictions_List);
   procedure Print_Feature_Values (Message : String; Rows : IL_Types.Rows_Vector;
                                   Column  : IL_Types.Class_Range);
   procedure Print_Feature_Types
     (Message : String; theTypes : Classifier_Utilities.Feature_Type_Array);
   procedure Print_Label_Types
     (Message : String; theTypes : Classifier_Utilities.Label_Type_Array);
   procedure Print_Leaf (Label_Counts : IL_Types.Predictions_List);
   procedure Print_Node (Node : IL_Types.Tree_Node_Type);
   procedure Print_Question (Message  : String;
                             Question : IL_Types.Question_Data);
   procedure Print_Raw_Question
     (Message : String; Question : IL_Types.Raw_Question);
   procedure Print_Row (Message : String; aRow : IL_Types.Row_Data);
   procedure Print_Row (Message : String; Rows : IL_Types.Rows_Vector;
                        Row_Number : Positive);
   procedure Print_Rows (Message : String; Rows : IL_Types.Rows_Vector);
   procedure Print_Tree (aTree : IL_Types.Tree_Package.Tree);
   procedure Print_UB_Label_Counts (Rows : IL_Types.Rows_Vector);
   procedure Print_Unique_Values (Rows    : IL_Types.Rows_Vector;
                                  Feature : IL_Types.Feature_Name_Type);
   procedure Print_Value_Record (Message : String;
                                 Value   : IL_Types.Value_Record);
   function Split_Row_Data (Row_Data : IL_Types.Rows_Vector)
                             return IL_Types.Data_Record;
   function Split_String (aString, Pattern : String)
                           return IL_Types.String_List;
   procedure Swap (Data : in out IL_Types.Value_Data_Lists_2D; L, R : Positive);
   pragma Inline (Swap);
   function XY_To_Rows (X, Y : IL_Types.Value_Data_Lists_2D)
                         return IL_Types.Rows_Vector;

end Utilities;
