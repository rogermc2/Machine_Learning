
with IL_Types;

package Coo is

   type Coo_Data_Type is (Coo_Dense, Coo_Sparse, Coo_MN, Coo_Data);

   type Shape_Dimensions is record
      Num_Rows : Positive;
      Num_Cols : Positive;
   end record;

   type Coo_Matrix (Data_Type : Coo_Data_Type := Coo_Sparse) is record
      Num_Dim : Positive;
      Shape   : Shape_Dimensions;
      case Data_Type is
         when Coo_Data =>
            Data           : IL_Types.Integer_List;
            Row_Indices    :  IL_Types.Integer_List;
            Column_Indices :  IL_Types.Integer_List;
         when others => null;
      end case;
   end record;

end Coo;
