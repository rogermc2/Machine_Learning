
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_8Aux is

   type Prediction_Kind is (True_Negative, True_Positive, False_Negative,
                            False_Positive);
   type Prediction_Info is array (Positive range <>) of Prediction_Kind;

   type Data_Record (Num_Items, Num_Features : Positive) is record
      Features : Real_Float_Matrix (1 .. Num_Items, 1 .. Num_Features);
      Labels   : Integer_Array (1 .. Num_Items);
   end record;

   function Comfort (Temp, Rel_Humid : Float) return Boolean;
   function Get_Predictions (Predictions : Boolean_Array; Labels : Boolean_Array)
                             return Prediction_Info;
   procedure Train_Test_Split
     (X          : Real_Float_Matrix; Y : Boolean_Array;
      Train_Size : Natural; Test_Size  : Natural;
      Train_X    : out Real_Float_Matrix; Train_Y : out Boolean_Array;
      Test_X     : out Real_Float_Matrix; Test_Y : out Boolean_Array);
   function Accuracy (Predictions : Boolean_Array; Labels : Boolean_Array)
                      return Float;

end Support_8Aux;
