
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Builder;
with ML_Types;

package Utilities is

   Utilities_Exception : exception;

   function Get_Data_Type (Data : Unbounded_String) return ML_Types.Data_Type;
   function Is_Boolean (Item : in Unbounded_String) return Boolean;
   function Is_Integer (Item : in Unbounded_String) return Boolean;
   function Is_Float (Item : in Unbounded_String) return Boolean;
   procedure Print_Best (Message : String; Best_Split : Builder.Best_Data);
   procedure Print_Classification
      (Classification : ML_Types.Prediction_Data_List);
   procedure Print_Node (Node : ML_Types.Tree_Node_Type);
   procedure Print_Question (Message : String;
                             Question : ML_Types.Question_Data);
   procedure Print_Raw_Question
      (Message : String; Question : ML_Types.Raw_Question);
   procedure Print_Rows (Message : String; Rows : ML_Types.Rows_Vector);
   procedure Print_Tree (aTree : ML_Types.Tree_Package.Tree);
   procedure Print_UB_Label_Counts (Rows : ML_Types.Rows_Vector);
   procedure Print_Unique_Values (Rows    : ML_Types.Rows_Vector;
                                  Feature : ML_Types.Feature_Name_Type);

end Utilities;
