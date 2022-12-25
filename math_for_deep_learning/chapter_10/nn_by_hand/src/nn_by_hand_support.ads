
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package NN_By_Hand_Support is

   procedure Build_Dataset (X_Train : out Real_Float_Matrix;
                            Y_Train : out Integer_Array;
                            X_Test  : out Real_Float_Matrix;
                            Y_Test  : out Integer_Array);

end NN_By_Hand_Support;
