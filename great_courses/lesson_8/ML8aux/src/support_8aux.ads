
with ML_Arrays_And_Matrices; use ML_Arrays_And_Matrices;

package Support_8Aux is

   type Data_Record (Num_Items, Num_Features : Positive) is record
      Features : Real_Float_Matrix (1 .. Num_Items, 1 .. Num_Features);
      Labels   : Integer_Array (1 .. Num_Items);
   end record;

   function Comfort (Temp, Rel_Humid : Float) return Boolean;
   procedure Train_Test_Split
     (X          : Real_Float_Matrix; Y : Boolean_Array;
      Train_Size : Natural; Test_Size  : Natural;
      Train_X    : out Real_Float_Matrix; Train_Y : out Boolean_Array;
      Test_X     : out Real_Float_Matrix; Test_Y : out Boolean_Array);
   function Test_Score (Predictions : Real_Float_Vector;
                        Labels      : Integer_Array) return Natural;

end Support_8Aux;
