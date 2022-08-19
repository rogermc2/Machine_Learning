
with Ada.Text_IO; use Ada.Text_IO;

package body Test_Support is

    function Almost_Equal (A, B : Real_Float_Matrix; Accuracy : Integer)
                           return Boolean is
        use Real_Float_Arrays;
        Diff   : constant Real_Float_Matrix := abs (A - B);
        Result : Boolean := True;
    begin
        for row in Diff'Range loop
            for col in Diff'Range (2) loop
                Result := Result and Diff (row, col) < 10.0 ** Accuracy;
            end loop;
        end loop;

        return Result;

    end Almost_Equal;

    --  ------------------------------------------------------------------------

    function Almost_Equal (A, B : Real_Float_Vector; Accuracy : Integer)
                           return Boolean is
        use Real_Float_Arrays;
        Diff   : constant Real_Float_Vector := abs (A - B);
        Result : Boolean := True;
    begin
        for row in Diff'Range loop
            Result := Result and Diff (row) < 10.0 ** Accuracy;
        end loop;

        return Result;

    end Almost_Equal;

    --  ------------------------------------------------------------------------

    function Almost_Equal (A, B : Stochastic_Optimizers.Parameters_Record;
                           Accuracy : Integer) return Boolean is
    begin

        return Almost_Equal (A.Coeff_Gradients, B.Coeff_Gradients, Accuracy) and
          Almost_Equal (A.Intercept_Grads, B.Intercept_Grads, Accuracy);

    end Almost_Equal;

    --  ------------------------------------------------------------------------

    procedure Print_Float_Array (Name  : String; anArray : Real_Float_Vector;
                                 Start : Integer := 1; Finish : Integer := 0) is
        Last  : Integer;
        Count : Integer := 1;
    begin
        if Finish > 0 then
            Last := Finish;
        else
            Last := Integer (anArray'Length);
        end if;

        Put_Line (Name & ": ");
        if Start >= anArray'First and then Finish <= anArray'Last then
            for Index in Start .. Last loop
                Put (Float'Image (anArray (Index)) & "  ");
                Count := Count + 1;
                if Count > 4 then
                    New_Line;
                    Count := 1;
                end if;
            end loop;
        else
            Put_Line ("Print_Float_Array called with invalid start or finish index.");
        end if;
        New_Line;

    end Print_Float_Array;

    --  ------------------------------------------------------------------------

    procedure Print_Float_Matrix
      (Name  : String; aMatrix : Real_Float_Matrix) is
    begin
        Put_Line (Name & ": ");
        for row in aMatrix'Range loop
            for col in aMatrix'Range (2) loop
                Put (Float'Image (aMatrix (row, col)) & "  ");
            end loop;
            New_Line;
        end loop;

    end Print_Float_Matrix;

    --  ------------------------------------------------------------------------

end Test_Support;
