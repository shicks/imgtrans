import matplotlib.pyplot as plt
import matplotlib.image as mpimg
import numpy as np
import math

def coef(s):
    if abs(s) < 1:
        return 1.5 * abs(s)**3 - 2.5 * s**2 + 1
    elif abs(s) < 2:
        return -.5 * abs(s)**3 + 2.5 * s**2 - 4 * abs(s) + 2
    return 0

def getpixel(img, x, y, r=1):
    """x and y are floats"""
    if x == round(x) and y == round(y):
        try:
            return img[int(x), int(y)]
        except IndexError:
            pass
    p = np.array([0, 0, 0], dtype = np.float32)
    for c in range(3):
        denom = 0.
        numer = 0.
        for xi in range(int(math.ceil(x - r)), int(math.floor(x + r)) + 1):
            xc = coef(x - xi) if xi >= 0 and xi < img.shape[0] else 0
            for yi in range(int(math.ceil(y - r)), int(math.floor(y + r)) + 1):
                yc = coef(y - yi) if yi >= 0 and yi < img.shape[1] else 0
                zc = xc * yc
                denom += zc
                numer += img[xi, yi, c] * zc if zc != 0 else 0
        p[c] = numer / denom if denom != 0 else 0
        if p[c] > 1:
            p[c] = 1
    if any(p >= 1):
        print (x, y, p)
        print img[int(math.ceil(x - 2)):int(math.floor(x + 2)) + 1,
                  int(math.ceil(y - 2)):int(math.floor(y + 2)) + 1]
    return p

def stretch(img, xs, ys):
    xl = int(xs*img.shape[0])
    yl = int(ys*img.shape[1])
    simg = np.zeros((xl, yl, 3), dtype = np.float32)
    for x in range(xl):
        print "x=%d" % (x,)
        for y in range(yl):
            simg[x, y] = getpixel(img, x / xs, y / ys)
    return simg

def funcinv(f, n):
    n0 = f(0)
    nt = int(round(f(n) - n0))
    xs = np.arange(n, dtype = np.float32)
    xt = np.arange(n + 1, dtype = np.float32)
    xinv = np.arange(nt, dtype = np.float32)
    for i in range(n):
        xt[i] = f(xs[i])
    xt[n] = f(n)
    # js0 = np.searchsorted(xt, np.arange(f(0), f(n), dtype = np.float32))
    for i in range(nt):
        j = np.searchsorted(xt, i + n0)
        if j >= 0 and j < n:
            xleft = xt[j]
            xright = xt[j + 1]
            xinv[i] = j + (i - xleft) / (xright - xleft)
        else:
            xinv[i] = j
    return xs, xt, xinv

def stretchf(img, xf, yf):
    x0 = xf(0); x1 = xf(img.shape[0]); xl = int(round(x1 - x0))
    y0 = yf(0); y1 = yf(img.shape[1]); yl = int(round(y1 - y0))
    simg = np.zeros((xl, yl, 3), dtype = np.float32)
    # need to invert...
    xs, xt, xinv = funcinv(xf, img.shape[0])
    ys, yt, yinv = funcinv(yf, img.shape[1])
    for x in range(xl):
        xi = xinv[x]
        print "x=%d, xi=%f" % (x,xi)
        for y in range(yl):
            yi = yinv[y]
            simg[x, y] = getpixel(img, xi, yi)
    return simg

def stretchfi(img, xl, yl, xf, yf):
    simg = np.zeros((xl, yl, 3), dtype = np.float32)
    for x in range(xl):
        xi = xf(x)
        print "x=%d, xi=%f" % (x,xi)
        for y in range(yl):
            yi = yf(y)
            simg[x, y] = getpixel(img, xi, yi)
    return simg

def distort(n):
    return lambda x:n/2. * (1 - math.cos(math.pi * x / n))

def edgestretch(a, b, n):
    """
    a is number of pixels in original
    b is number of pixels in stretched edge
    n is total number of pixels of original
    """
    ratio = float(b-a)/a
    def f(x, a=a, b=b, n=n):
        if x >= a and x < n - a:
            return x + b - a
        z = (0, 1, x) if x<a else (n+2*b-2*a, -1, n-x)
        return z[0] + z[1] * (
            float(b)*z[2]/a + (b-a)/math.pi * math.sin(math.pi*z[2]/a))
    return f

#img = plt.imread('stinkbug.png')
#scaled4 = stretch(img, 0.8, 1.2)

def inverse(f, n):
    # NOTE: compute exact roots and then memoize
    pass

def square(img, stretchfactor = 2):
    # stretchfactor = b/a - lower is less severe distortion for longer
    x = img.shape[0]
    y = img.shape[1]
    diff = (x-y)/2
    dx = diff * (1.*x/(x+y))
    dy = -diff * (1.*y/(x+y))
    ax = int(round(1.5 * abs(dx) + dx / 2))
    bx = int(round(1.5 * abs(dx) - dx / 2))
    ay = int(round(1.5 * abs(dy) + dy / 2))
    by = int(round(1.5 * abs(dy) - dy / 2))
    return stretchf(img, edgestretch(ax, bx, x), edgestretch(ay, by, y))

def square_i(img, stretchfactor = 2):
    x = img.shape[0]
    y = img.shape[1]
    diff = (x-y)/2
    dx = diff * (1.*x/(x+y))
    dy = -diff * (1.*y/(x+y))
    ax = int(round(1.5 * abs(dx) + dx / 2))
    bx = int(round(1.5 * abs(dx) - dx / 2))
    ay = int(round(1.5 * abs(dy) + dy / 2))
    by = int(round(1.5 * abs(dy) - dy / 2))
    print (x, ax, bx, y, ay, by)
    return stretchfi(img, x + 2*(bx-ax), y + 2*(by-ay),
                     edgestretch(bx, ax, x + 2*(bx-ax)),
                     edgestretch(by, ay, y + 2*(by-ay)))

def saveimage(fname, arr, vmin=None, vmax=None, cmap=None, format=None, origin=None):
    from matplotlib.backends.backend_agg import FigureCanvasAgg as FigureCanvas
    from matplotlib.figure import Figure

    fig = Figure(figsize=[arr.shape[1], arr.shape[0]], dpi=1, frameon=False)
    canvas = FigureCanvas(fig)
    fig.figimage(arr, cmap=cmap, vmin=vmin, vmax=vmax, origin=origin)
    fig.savefig(fname, dpi=1, format=format)
