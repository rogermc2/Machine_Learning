
with Multiclass_Utils; use Multiclass_Utils;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

generic
   type Index_Type is range <>;  --  Any signed integer type
   type Class_Type is private;
   type Y_Array_Type is array (Index_Type range <>) of Class_Type;
   type Class_Array_Type is array (Index_Type range <>) of Class_Type;
   with function Type_Of_Target (Y : Y_Array_Type) return Y_Type;
   with function "<" (L, R : Class_Type) return Boolean is <>;
package Generic_Label_Binarize is

   function Label_Binarize_G (Y         : Y_Array_Type;
                              Classes   : Class_Array_Type;
                              Neg_Label : Integer := 0;
                              Pos_Label : Integer := 1) return Binary_Matrix;
end Generic_Label_Binarize;
