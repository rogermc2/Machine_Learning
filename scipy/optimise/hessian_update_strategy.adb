--  Based on /scipy/scipy/optimize/_hessian_update_strategy.py

with Ada.Assertions; use Ada.Assertions;
package body Hessian_Update_Strategy is

    procedure Initialize (Self : in out Update_Strategy; Num_Dim : Positive;
                         Approx_Type : Hessian_Approx_Type) is
    begin
        Self.Approx_Type := Approx_Type;
        Self.Dim := Num_Dim;
        Assert (False, "Hessian_Update_Strategy.Initialize not implemented");
    end Initialize;

end Hessian_Update_Strategy;
