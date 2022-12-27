
with Ada.Containers.Ordered_Maps;

with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;
with ML_Types;

package Network is

   package Network_Package is new Ada.Containers.Ordered_Maps
     (Key_Type => String_2, Element_Type => Float);

   type Dataset (Train_Length, Test_Length, Num_Features : Positive) is record
      X_Train : Real_Float_Matrix (1 .. Train_Length, 1 .. Num_Features);
      Y_Train : Integer_Array (1 .. Train_Length);
      X_Test  : Real_Float_Matrix (1 .. Test_Length, 1 .. Num_Features);
      Y_Test  : Integer_Array (1 .. Test_Length);
   end record;

   function Build_Dataset return Dataset;
   procedure Evaluate
     (Net            : Network_Package.Map; X : Real_Float_Matrix;
      Y              : Integer_Array;
      Tn, Fp, Fn, Tp : out Natural; Accuracy : out Float;
      Pred           : out ML_Types.Integer_List);
   procedure Gradient_Descent
     (Net : in out Network_Package.Map; X   : Real_Float_Matrix;
      Y   : Integer_Array; Epochs : Positive; Eta : Float);
   function Load_Data (File_Name : String; Num_Columns : Positive)
                       return Real_Float_Matrix;
   function Load_Data (File_Name : String) return Real_Float_Vector;
   function Neural_Net return Network_Package.Map;

end Network;
