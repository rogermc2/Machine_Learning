
--  with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Support_4 is

   type Base_State (Num_Train, Num_Test, Num_Features : Positive) is record
      Train_X : Float_Matrix (1 .. Num_Train, 1 .. Num_Features);
      Train_Y : Integer_Matrix (1 .. Num_Train, 1 .. 1);
      Test_X  : Float_Matrix (1 .. Num_Test, 1 .. Num_Features);
      Test_Y  : Integer_Matrix (1 .. Num_Test, 1 .. 1);
   end record;

   function Get_State
     (Dataset_Name : String;
      Test_Size, Train_Size : Positive) return Base_State;
--     function Get_Classifier
--       (Dataset_Name : String;
--        Classifier   : out Multilayer_Perceptron.MLP_Classifier)
--        return Boolean;
--     procedure Save_Classifier
--       (Dataset_Name : String; Classifier : Multilayer_Perceptron.MLP_Classifier);

end Support_4;
