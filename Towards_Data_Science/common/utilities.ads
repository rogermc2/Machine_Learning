
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Builder;
with ML_Types;

package Utilities is

   Utilities_Exception : exception;

   function Get_Data_Type (Data : Unbounded_String) return ML_Types.Data_Type;
   function Is_Boolean (Item : in Unbounded_String) return Boolean;
   function Is_Integer (Item : in Unbounded_String) return Boolean;
   function Is_Float (Item : in Unbounded_String) return Boolean;
   procedure Print_Best (Self : Builder.Best_Data);
   procedure Print_Classification
     (Classification : ML_Types.Count_Package.Map);
   function Print_Leaf (Counts : ML_Types.Count_Package.Map) return String;
   procedure Print_Question (Label : String; Self : ML_Types.Question_Data);
   procedure Print_Raw_Question (Self : ML_Types.Raw_Question);
   procedure Print_Rows (Label : String; Rows : ML_Types.Rows_Vector);
   procedure Print_Tree (aTree : ML_Types.Tree_Package.Tree);
   procedure Print_UB_Class_Counts (Rows : ML_Types.Rows_Vector);

end Utilities;
