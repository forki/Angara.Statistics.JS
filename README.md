##Angara.Statistics translated with WebSharper

Open and build Angara.Statistics.JS.sln


#### Caution! The code is partially rewritten to satisfy WebSharper capabilities and is not yet fully tested!


For now the following functions are tranlated and can be used from JavaScript:
*   Statistics.kde - Gaussian kernel density estimator for one-dimensional data
*   Statistics.kde2
*   Statistics.ridders - Root of a function using Ridders method
*   Statistics.dct - Descrete cosine transform
*   Statistics.idct - Inverse discrete cosine transform
*   Statistics.fft - Fast Fourier transform
*   Statistics.ifft - Inverse Fast Fourier Transform    

The project also contains helper function *StatisticsHelpers.getTableViewerSource* that takes table data and using the functions above prepares the data structure to activate TableViewer.JS from Angara.Table project.

See the [demo](http://itislab.github.io/Angara.Statistics.JS/table_stats_sample/)
