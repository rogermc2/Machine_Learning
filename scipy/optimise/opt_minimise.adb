--  Based on scipy/optimise/_minimize.py

with Ada.Text_IO; use Ada.Text_IO;

with L_BFGS_B;
with NL_Types;
with NL_Arrays_And_Matrices; use NL_Arrays_And_Matrices;

package body Opt_Minimise is

   function Optimize_Result_For_Equal_Bounds
     (Fun    : Multilayer_Perceptron.Loss_Grad_Access;
      Args   :  Multilayer_Perceptron.Loss_Grad_Args;
      Bounds : Constraints.Bounds_List)
      return Optimise.Optimise_Result;
   --  Standardize_Bounds converts bounds to the form required by the solver
   --     function Standardize_Bounds (Bounds : Constraints.Bounds_List;
   --                                  X0     : Stochastic_Optimizers.Parameters_List;
   --                                  Method : Method_Type)
   --                                  return Constraints.Bounds_List;

   --  -------------------------------------------------------------------------

   function Check_Options return Boolean is
   begin
      return True;

   end Check_Options;

   --  -------------------------------------------------------------------------
   --  L45 Minimization of scalar function of one or more variables.
   --  Fun: The objective function to be minimized
   --  fun(x, *args) -> float
   --  x is a 1-D array with shape (n,) and ``args`` is a list of the
   --  fixed parameters needed to completely specify the function.
   --  x0 : ndarray, shape (n,) is an initial guess array of real elements of
   --  size (n,);  n is the number of independent variables.
   procedure Minimise (Fun        : Multilayer_Perceptron.Loss_Grad_Access;
                       Args       : Multilayer_Perceptron.Loss_Grad_Args;
                      X0          : Stochastic_Optimizers.Parameters_List;
                      Result      : in out Optimise.Optimise_Result;
                      Method      : Method_Type := No_Method;
                      Jac         : Num_Diff.FD_Methods := Num_Diff.FD_None;
                      Bounds      : Constraints.Bounds_List :=
                        Constraints.Array_Bounds_Package.Empty_Vector;
                      Constraints : Minimise_Constraints_List :=
                        Minimise_Constraints_Package.Empty_List) is
--                        Options     : Minimise_Options := No_Options)
      use Minimise_Constraints_Package;
      use Num_Diff;
      Routine_Name : constant String := "Opt_Minimise.Minimise ";
      L_Method    : Method_Type := Method;
      I_Fixed     : NL_Types.Boolean_List;
      All_Fixed   : Boolean := True;
      --  L655 determine if finite differences are needed for any grad or jac
      FD_Needed   : Boolean := Jac /= Fd_Callable;
      Cons_Cursor : Cursor := Constraints.First;
      Remove_Vars : Boolean := False;
      Done        : Boolean := False;
   begin
      --  L531
      if L_Method = No_Method then
         if not Constraints.Is_Empty then
            L_Method := Slsqp_Method;
         elsif not Bounds.Is_Empty then
            L_Method := L_BFGS_B_Method;
         else
            L_Method := BFGS_Method;
         end if;
      end if;

      --  L531
      if Check_Options then
         --  L632
         if not Bounds.Is_Empty then
            if L_Method = Tnc_Method or L_Method = Slsqp_Method or
              L_Method = L_BFGS_B_Method then
               --  L645
               for index in Bounds.First_Index .. Bounds.Last_Index loop
                  I_Fixed.Append (Bounds (index).Upper = Bounds (index).Lower);
                  All_Fixed := All_Fixed and I_Fixed (index);
               end loop;

               Done := All_Fixed;
               if All_Fixed then
                  Result := Optimize_Result_For_Equal_Bounds (Fun, Args, Bounds);
               else
                  while Has_Element (Cons_Cursor) loop
                     if Element (Cons_Cursor) /= Callable_Constraint then
                        FD_Needed := True;
                        for index in I_Fixed.First_Index
                          .. I_Fixed.Last_Index loop
                           Remove_Vars := Remove_Vars or I_Fixed (index);
                        end loop;
                        Remove_Vars := Remove_Vars or FD_Needed or
                          Method = Tnc_Method;
--                          if Remove_Vars then
--                             null;
--                          end if;
                     end if;
                     Next (Cons_Cursor);
                  end loop;

               end if;
            end if;
         end if;  --  not Bounds.Is_Empty
         Put_Line (Routine_Name & "not Done test");

         if not Done then
            case Method is
               when L_BFGS_B_Method =>
                  L_BFGS_B.Minimise_LBFGSB
                    (Fun => Fun, X0 => X0, Result => Result, Bounds => Bounds);
               when others => null;
            end case;
         end if;

--           if Remove_Vars then
--              null;
--           end if;
      end if;  --  Check_Options

   end Minimise;

   --  -------------------------------------------------------------------------
   --  L977
--     Loss_Grad_LBFGS
--        (Self        : in out Multilayer_Perceptron.MLP_Classifier;
--         Params      : Stochastic_Optimizers.Parameters_List;
--         X           : Real_Float_Matrix;
--         Y           : Boolean_Matrix;
--         Activations : in out Real_Matrix_List;
--         Gradients   : out Stochastic_Optimizers.Parameters_List)
--         return Float;
   function Optimize_Result_For_Equal_Bounds
     (Fun    : Multilayer_Perceptron.Loss_Grad_Access;
      Args   :  Multilayer_Perceptron.Loss_Grad_Args;
      Bounds : Constraints.Bounds_List)
      return Optimise.Optimise_Result is
      Success : constant Boolean := True;
      X0      : Real_Float_Vector (1 .. Positive (Bounds.Length));
      --  L1013
      Result  : Optimise.Optimise_Result (0, 0, 0);
   begin
      for index in Bounds.First_Index .. Bounds.Last_Index loop
         X0 (index) := Bounds (index).Lower;
      end loop;

      Result.Fun := Fun (Args);
      Result.X := X0;
      Result.N_Fev := 1;
      Result.Success := Success;
      return Result;

   end Optimize_Result_For_Equal_Bounds;

   --  -------------------------------------------------------------------------

   --     function Standardize_Bounds
   --       (Bounds : Constraints.Bounds_List;
   --        X0     : Stochastic_Optimizers.Parameters_List;
   --        Method : Method_Type) return Constraints.Bounds_List is
   --        New_Bounds : Constraints.Bounds_List;
   --     begin
   --        case Method is
   --           when Trust_Constr_Method | Powell_Method | Nelder_Mead_Method
   --              | New_Method => null;
   --           when L_BFGS_B_Method | Tnc_Method | Slsqp_Method | Old_Method=> null;
   --           when others => null;
   --        end case;
   --
   --        return New_Bounds;
   --
   --     end Standardize_Bounds;

   --  -------------------------------------------------------------------------

end Opt_Minimise;
