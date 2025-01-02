
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import numpy as np

# Create sine plot
plt.figure(figsize=(10, 6))
x = np.linspace(0, 4*np.pi, 200)
y = np.sin(x)
plt.plot(x, y, 'b-', label='sin(x)')
plt.grid(True)
plt.title('Sine Wave')
plt.xlabel('x')
plt.ylabel('y')
plt.legend()
plt.savefig('results/figures/sine.jpg', bbox_inches='tight', dpi=300)
plt.close()

# Create sine and cosine plot
plt.figure(figsize=(10, 6))
y2 = np.cos(x)
plt.plot(x, y, 'b-', label='sin(x)')
plt.plot(x, y2, 'r--', label='cos(x)')
plt.grid(True)
plt.title('Sine and Cosine Waves')
plt.xlabel('x')
plt.ylabel('y')
plt.legend()
plt.savefig('results/figures/sine_cosine.jpg', bbox_inches='tight', dpi=300)
plt.close()
