import numpy as np
import matplotlib.pyplot as plt

cases = [
    {"sigma2": 2, "tau2": 4, "label": r"$\sigma^2=2, \tau^2=4$"},
    {"sigma2": 3, "tau2": 3, "label": r"$\sigma^2=3, \tau^2=3$"},
    {"sigma2": 4, "tau2": 2, "label": r"$\sigma^2=4, \tau^2=2$"},
]
n = np.linspace(1, 100, 100)
plt.figure(figsize=(10, 6))
for case in cases:
    sigma2 = case["sigma2"]
    tau2 = case["tau2"]
    m_values = 64 * (tau2 + sigma2 / n)
    plt.plot(n, m_values, label=case["label"])
plt.xlabel("n")
plt.ylabel("Lower Bound for m")
plt.title("Lower Bound for m as a Function of n")
plt.legend()
plt.grid(True)
plt.show()