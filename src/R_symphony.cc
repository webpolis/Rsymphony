/*-------------------------------------------------------------------------*/
/*
  This is an example of using SYMPHONY to construct and solve 
  a simple MILP.
  Therefore 2 functions are defined which are prepared to be compiled as an
  shared object and then called by R with all the parameters that set up
  the linear problem.
*/

/*-------------------------------------------------------------------------*/

#include <coin/symphony.h>
#include <R.h>

extern "C" {

void R_symphony_solve(int *n_cols, int *n_rows, int *start, int *index, 
		      double *value, double *col_lb, double *col_ub,
		      int* is_int, double *objective, double *obj2,
		      char **row_sense, double *row_rhs,
		      double *row_range, double *obj_final,
		      double *sol_final, int *solve_status)
{
   int i;
   
   /* Create a SYMPHONY environment */
   sym_environment *env = sym_open_environment();
   
   /* Set verbosity to -2 to have no output from the SYMPHONY
      application at all (default = 0).
   */
   sym_set_int_param(env, "verbosity", -2);

   /* Set if integer */
   char * int_vars = (char *) malloc (sizeof(char) * (*n_cols));
   for(i=0; i < (*n_cols); i++)
       if(is_int[i] == 1)
	   int_vars[i] = TRUE;
       else
	   int_vars[i] = FALSE;

   /* Load the problem in the symphony environment after all variables
      are set.
   */
   sym_explicit_load_problem(env, *n_cols, *n_rows, start, index, value,
			     col_lb, col_ub, int_vars, objective, NULL,
			     *row_sense, row_rhs, row_range, TRUE);
 			     			     
   /* Solve the integer program. */
   sym_solve(env);

   /* Get the solution. */
   double * solution = (double *) malloc (sizeof(double) * (*n_cols));
   double objective_value = 0.0;

   sym_get_col_solution(env, solution);
   sym_get_obj_val(env, &objective_value);
   
   *obj_final = objective_value;
   for(i=0; i < (*n_cols); i++)	
     sol_final[i] = solution[i];

   *solve_status = sym_get_status(env);

   sym_close_environment(env);
}
}
