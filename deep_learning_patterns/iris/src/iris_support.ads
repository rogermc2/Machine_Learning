
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Iris_Support is

   type Dataset (Train_Length, Test_Length, Num_Features : Positive) is record
      X_Train : Real_Float_Matrix (1 .. Train_Length, 1 .. Num_Features);
      Y_Train : Real_Float_Matrix (1 .. Train_Length, 1 .. 1);
      X_Test  : Real_Float_Matrix (1 .. Test_Length, 1 .. Num_Features);
      Y_Test  : Real_Float_Matrix (1 .. Test_Length, 1 .. 1);
   end record;

   function Build_Dataset (Train_Length : Positive := 70;
                           Test_Length  : Positive := 30) return Dataset;

end Iris_Support;
