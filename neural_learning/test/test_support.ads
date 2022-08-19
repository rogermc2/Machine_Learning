
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Test_Support is

    procedure Print_Float_Array (Name  : String; anArray : Real_Float_Vector;
                                 Start : Integer := 1; Finish : Integer := 0);
    procedure Print_Float_Matrix (Name  : String; aMatrix : Real_Float_Matrix);

end Test_Support;
