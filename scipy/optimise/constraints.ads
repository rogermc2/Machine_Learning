--  Based on scipy/optimise/_constraints.py

with NL_Types;

package Constraints is

    type Array_Bounds is record
        Lower : NL_Types.Integer_List;
        Upper : NL_Types.Integer_List;
    end record;

end Constraints;
