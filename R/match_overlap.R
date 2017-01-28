
match_overlap = function(){
    UseMethod("match_overlap")
}


match_overlap.spectra = function(){

}


i_match_overlap_svc = function(){

    # Here's the matching algorithms used for the SVC.
    #
    # ---------- Forwarded message ----------
    # From: "Lawrence Slomer" <lslomer@spectravista.com>
    # Date: Jun 12, 2015 9:07 AM
    # Subject: Re: Fwd: Re: SVC HR-1024i Matching Algorithm
    # To: <frede368@umn.edu>
    # Cc: <tomc@spectravista.com>
    #
    # Brett -
    #
    # Good day. Tom Corl asked me to respond to your matching question.
    #
    # The main matching algorithm uses the average values of the
    # Si and SWIR1 radiance values within the transition region
    # to compute a scalar "matching factor". This matching factor
    # is then used to scale the Si region up or down, so that the
    # Si region data (which is not temperature controlled) is matched
    # to the more stable SWIR1 data (which *is* temperature controlled).
    #
    # This compensates for sensitivity changes in the Si detector
    # with temperature. This algorithm is based on the physics of the Si
    # detector. The matching factor affects all of the Si
    # detector data, but is scaled to have less effect at lower wavelengths
    # since Si sensitivity is affected less by temperature at lower wavelengths.
    #
    # The NIR-SWIR matching, if checked, is a purely cosmetic
    # further cleanup of the *just* the transition region radiance.
    # It has no effect outside the transition region. It simply combines
    # the Si and SWIR1 data mathematically on a sliding scale so that
    # the resulting data does not have a step. As I mentioned this is
    # purely cosmetic and does not have any basis in the physics of
    # the detectors.
    #
    # Matching is generally not necessary if your application is only
    # interested in reflectance, as long as you have let the instrument
    # stabilize and take reference scans at reasonable intervals. A small
    # step between Si and SWIR1 due to temperature will divide out (generally)
    # and result in smooth reflectance in that region.
    #
    # Matching requires "reasonable" amounts of energy in the transition
    # region; otherwise noise may cause the algorithm to incorrectly
    # shift the Si radiance values.
    #
    # Please let me know if this answers your question.
    #
    # --Larry
}
