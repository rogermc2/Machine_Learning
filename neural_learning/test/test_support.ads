
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;
with Stochastic_Optimizers;

package Test_Support is

    function Almost_Equal (A, B : Real_Float_Matrix; Accuracy : Integer)
                           return Boolean;
    function Almost_Equal (A, B : Real_Float_Vector; Accuracy : Integer)
                           return Boolean;
    function Almost_Equal (A, B : Stochastic_Optimizers.Parameters_Record;
                           Accuracy : Integer) return Boolean;
    procedure Print_Float_Array (Name  : String; anArray : Real_Float_Vector;
                                 Start : Integer := 1; Finish : Integer := 0);
    procedure Print_Float_Matrix (Name  : String; aMatrix : Real_Float_Matrix);
   procedure Print_Float_Vector (Name : String; Vec : Real_Float_Vector);

end Test_Support;
