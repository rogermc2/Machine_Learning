
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package Hessian_Update_Strategy is
    use Real_Float_Arrays;

    type Hessian_Approx_Type is (Approx_Hess, Approx_Inv_Hess);

    type Update_Strategy is record
        Approx_Type : Hessian_Approx_Type;
        Dim         : Positive;
    end record;

    type Full_Update_Strategy (Dim1, Dim2 : Positive) is record
        Approx_Type : Hessian_Approx_Type;
        Dim         : Positive;
        First_Iter  : Natural := 0;
        B           : Real_Float_Matrix (1 .. Dim1, 1 .. Dim2) :=
                        Unit_Matrix (Dim1);
        H           : Real_Float_Matrix (1 .. Dim1, 1 .. Dim2) :=
                        Unit_Matrix (Dim1);
    end record;

    procedure Initialize (Self : in out Update_Strategy; Num_Dim : Positive;
                         Approx_Type : Hessian_Approx_Type);

end Hessian_Update_Strategy;
