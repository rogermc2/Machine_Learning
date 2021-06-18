
with Builder;
with ML_Types;

package Utilities is

   procedure Print_Best (Self : Builder.Best_Data);
   procedure Print_Classification
     (Classification : ML_Types.Count_Package.Map);
   function Print_Leaf (Counts : ML_Types.Count_Package.Map) return String;
   procedure Print_Question (Self : ML_Types.Question_Data);
   procedure Print_Raw_Question (Self : ML_Types.Raw_Question);
   procedure Print_Rows (Label : String; Rows : ML_Types.Rows_Vector);
   procedure Print_Tree (aTree : ML_Types.Tree_Package.Tree);
   procedure Print_UB_Class_Counts (Rows : ML_Types.Rows_Vector);

end Utilities;
