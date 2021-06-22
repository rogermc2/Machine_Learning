
with Ada.Containers.Indefinite_Multiway_Trees;
with Ada.Text_IO; use Ada.Text_IO;

procedure Math_Tree is

    type Operator is (plus, minus, times, Div);
    type Element_Kind is (Op, Variable, Literal);
    subtype Variable_Name is Character range  'a' .. 'z';

    type Element_Type (E_Kind : Element_Kind) is
       record
           case E_Kind is
           when Op =>
               Math_Function : Operator;
           when Variable =>
               Var           : Character;
           when Literal =>
               Value         : Float;
           end case;
       end record;

    package Expression_Trees is
      new Ada.Containers.Indefinite_Multiway_Trees (Element_Type);
    use Expression_Trees;

    My_Tree : Tree := Empty_Tree;
    Variables : array (Variable_Name) of Float := (others => 0.0);
    Curs      : Cursor := Root (My_Tree);
    X         : Float;

    function Eval (Curs : Cursor) return Float is
        Elem : Element_Type := Element (Curs);
        L, R : Float;
    begin
        case Elem.E_Kind is
        when Op =>
            L := Eval (First_Child (Curs));
            R := Eval (Last_Child (Curs));
            case Elem.Math_Function is
            when plus => return (L + R);
            when minus => return (L - R);
            when times => return (L * R);
            when div => return (L / R);
--              when others => return 0.0;
            end case;
        when Variable =>
            return Variables (Elem.Var);
        when Literal =>
            return Elem.Value;
        end case;
    end Eval;

begin
    --  Top node  L1
    Insert_Child
      (Container => My_Tree,
       Parent => Curs,
       Before => No_Element,
       New_Item => (Op, times),
       Position => Curs);

    --  2nd level L1.1, L1.2
    My_Tree.Insert_Child (Curs, No_Element, (Op, plus));
    My_Tree.Insert_Child (Curs, No_Element, (Op, minus));

    -- 3rd level left L1.1.1, right L1.1.2
    Curs := First_Child (Curs);
    My_Tree.Insert_Child (Curs, No_Element, (Variable, 'x'));
    My_Tree.Insert_Child (Curs, No_Element, (Literal, 3.0));

    -- 3rd level right (L1.2.1, L1.2.2)
    Curs := Next_Sibling (Curs);
    My_Tree.Insert_Child (Curs, No_Element, (Variable, 'y'));
    My_Tree.Insert_Child (Curs, No_Element, (Literal, 4.0));

    X := Eval (First_Child(My_Tree.Root));
    Put_Line ("Result: " & Float'Image (X));
end Math_Tree;
