posterior <- function(t) {
    0.5 * exp(-0.5 * (t + 3)^2) + 0.5 * exp(-0.5 * (t - 3)^2)
}

metropolis_cpp <- Rcpp::cppFunction('
Rcpp::List metropolis(Function target, double proposal_std, int niter,
                      int burn_in, int thinning)
{
    std::vector<double> chain(niter);
    int accepted = 0;
    double y, x = 0.0;
    double acc_prob;

    for (int i = 0; i < niter; ++i) {
        y = rnorm(1, x, proposal_std)[0];

        // acceptance probability
        acc_prob = std::min(1.0, *REAL(target(y)) / *REAL(target(x)));

        // metropolis test
        if (runif(1)[0] < acc_prob) {
            x = y;
            ++accepted;
        }

        // store in the chain
        chain[i] = x;
    }

    // discard burn-in
    chain.erase(chain.begin(), chain.begin() + burn_in);

    // thinning
    int count = 0;
    for (auto it = chain.begin(); it != chain.end(); ) {
        if (count % thinning != 0) {
            it = chain.erase(it);
        } else {
            ++it;
        }
        ++count;
    }

    return List::create(_["chain"] = chain,
                        _["acc_rate"] = double(accepted) / niter);
}
')

metropolis_r <- function(target, proposal_std, niter, burn_in, thinning) {
    chain <- numeric(niter)
    accepted <- 0
    x <- 0

    for (i in 1:niter) {
        y <- rnorm(1, x, proposal_std)

        acc_prob <- min(1, target(y) / target(x))
        if (runif(1) < acc_prob) {
            x <- y
            accepted <- accepted + 1
        }

        chain[i] <- x
    }

    chain <- chain[seq(burn_in, length(chain), by = thinning)]

    list(chain = chain, acc_rate = accepted / niter)
}

niter <- 1e4
burn_in <- 1000
thinning <- 10
std <- 1

rbenchmark::benchmark(
    r = metropolis_r(posterior, std, niter, burn_in, thinning),
    cpp = metropolis_cpp(posterior, std, niter, burn_in, thinning)
)
