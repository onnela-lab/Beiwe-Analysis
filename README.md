
## Project name
Beiwe data analysis code

## Description
This repository contains an evolving code base for running various types of analyses on data collected with the Beiwe research platform for smartphone-based digital phenotyping. We will merge several distinct projects into this single repository by the end of 2018. Note that this repository is distinct from the Beiwe data anlaysis pipeline. For documentation of Beiwe back-end, [start here](https://github.com/onnela-lab/beiwe-backend/wiki).

## Documentation
This is being updated but you can [start here](https://github.com/onnela-lab/Beiwe-Analysis/wiki/Documentation). We will migrate documentation over to [Sphinx](http://www.sphinx-doc.org/en/master/), which is the most widely used documentation generator written and used by the Python community. 

## License
This repository is published under the 3-clause BSD 3-clause, see [here](LICENSE.md).

## Contribute
We would love your contribution! To avoid fragmentation of the code base and maximize its usability, we have decided on the following guidelines:
* All code should be placed on GitHub in this repository
* All new functionalities should be implemented in Python (old ones are being converted into Python)
* For computationally intensive parts, use NumPy and SciPy, possibly consider C extension modules
* Use [Google's style guide for Python](https://github.com/google/styleguide/blob/gh-pages/pyguide.md)
* Comment your functions generously using that same guideline (section 3.8 Comments and Docstrings)
* Do use reStructuredText (reST) in your docstring, it helps with documentation generators

## Credits
The Beiwe platform was developed by the Onnela Lab at Harvard University. Several individuals have contributed to this project. See the lab's [web page](https://www.hsph.harvard.edu/onnela-lab/) for more information.
