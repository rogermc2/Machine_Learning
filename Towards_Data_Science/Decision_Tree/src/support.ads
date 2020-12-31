
with ML_Types; use ML_Types;

package Support is

   function Class_Counts (Rows : Rows_Vector) return Count_Package.Map;
   function Gini (Rows : Rows_Vector) return Float;
   function Information_Gain (Left, Right : Rows_Vector;
                              Current_Uncertainty : Float) return float;
   procedure Print_Question (Self : Question_Type);
   procedure Print_Rows (Label : String; Rows : Rows_Vector);
   procedure Print_Tree (aTree : Tree_Package.Tree);
   function To_Vector (Rows : Row_Array) return Rows_Vector;

end Support;
