
with Multilayer_Perceptron;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Openml_Ada;

package Support_4 is

   function Get_State
     (Dataset_Name : String;
      Train_X      : out Float_Matrix;
      Train_Y      : out Integer_Matrix;
      Test_X       : out Float_Matrix;
      Test_Y       : out Integer_Matrix;
      Bunch        : out Openml_Ada.Bunch_Data) return Boolean;
   function Get_Classifier
     (Dataset_Name : String;
      Classifier   : out Multilayer_Perceptron.MLP_Classifier)
      return Boolean;
   procedure Save_State
     (Dataset_Name : String;
      Train_X      : Float_Matrix;
      Train_Y      : Integer_Matrix;
      Test_X       : Float_Matrix;
      Test_Y       : Integer_Matrix;
      Save_Bunch   : Openml_Ada.Bunch_Data);
   procedure Save_Classifier
     (Dataset_Name : String; Classifier : Multilayer_Perceptron.MLP_Classifier);

end Support_4;
