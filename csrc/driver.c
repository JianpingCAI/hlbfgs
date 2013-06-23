#include <stdlib.h>
#include <string.h>

#ifdef NO_TRAILING_UNDERSCORE
extern void lbfgs();
#define LBFGS lbfgs
#else
extern void lbfgs_();
#define LBFGS lbfgs_
#endif

struct lbfgs_state
{
	int N, M, idiag, iflag;
	double eps, xtol;
	double F, *G, *X, *W, *DIAG;
};
typedef struct lbfgs_state state_t;

double *get_solution_vector(state_t *state)
{
	return state->X;
}

double *get_solution_vector_copy(state_t *state)
{
	double *X = malloc(state->N * sizeof(double));
	if (X == NULL)
		return NULL;
	memcpy(X, state->X, state->N * sizeof(double));
	return X;
}

void free_solution_vector_copy(double *X)
{
	free(X);
}

void update_cost_and_gradient(state_t *state, double F, double *G)
{
	state->F = F;
	memcpy(state->G, G, state->N * sizeof(double));
}

double machine_epsilon()
{
	int i;
	double x = 1.0;
	for (i = 0; i < 52; i++)
		x /= 2;
	return x;
}

state_t *initialize_solver(int N, int M, double eps, double *X0)
{
	size_t worksize = (size_t)(N) * (2 * M + 1) + 2 * M;
	state_t *state;
	if ((state = malloc(sizeof(*state))) == NULL)
		goto err0;
	state->idiag = 0;
	state->iflag = 0;
	state->N = N;
	state->M = M;
	state->eps = eps;
	state->xtol = machine_epsilon();
	if ((state->X = malloc(N * sizeof(double))) == NULL)
		goto err1;
	if ((state->G = malloc(N * sizeof(double))) == NULL)
		goto err2;
	if ((state->W = malloc(worksize * sizeof(double))) == NULL)
		goto err3;
	if ((state->DIAG = malloc(N * sizeof(double))) == NULL)
		goto err4;
	memcpy(state->X, X0, N * sizeof(double));
	return state;
	/* unwind all of the allocations */
err4:	free(state->W);
err3:	free(state->G);
err2:	free(state->X);
err1:	free(state);
err0:	return NULL;
}

void *finalize_solver(state_t *state)
{
	free(state->DIAG);
	free(state->W);
	free(state->G);
	free(state->X);
	free(state);
}

int iterate_solver(state_t *state)
{
	int iprint[] = {-1,0};
	/*  SUBROUTINE LBFGS(N,M,X,F,G,DIAGCO,DIAG,IPRINT,EPS,XTOL,W,IFLAG) */
	LBFGS(&state->N, &state->M, state->X, &state->F, state->G,
	      &state->idiag, state->DIAG, iprint, &state->eps, &state->xtol, state->W,
	      &state->iflag);
	return state->iflag;
}
