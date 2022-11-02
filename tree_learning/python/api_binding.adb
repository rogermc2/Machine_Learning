
with Interfaces.C;

package body API_Binding is

    procedure API_2D (A, B : aliased Integer_Matrix;
                      A_Ptrs, B_Ptrs : out API_Int_Pointer_Array) is

        First_A_Col : constant Integer := A'First (2);
        First_B_Col : constant Integer := B'First (2);
        A_Value     : aliased Integer;
        B_Value     : aliased Integer;
    begin
        for row in A'range loop
            A_Value := A (row, First_A_Col);
            A_Ptrs (row) := A_Value'Unchecked_Access;
        end loop;

        for row in B'range loop
            B_Value := B (row, First_B_Col);
            B_Ptrs (row) := B_Value'Unchecked_Access;
        end loop;

    end API_2D;

end API_Binding;
