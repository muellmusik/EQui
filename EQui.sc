// ht to Wouter Snoei
EQui : QUserView {
	var params, <target, prefix, sampleRate;

	*viewClass { ^QUserView }

	*new { arg parent, bounds, target, params, prefix = "", sampleRate;
		^super.new(parent, bounds).init(target, params, prefix, sampleRate);
	}

	*sizeHint {
		^Point(300,200);
	}

	target_ {|intarget| target = intarget; target.set(*params.asArgsArray(prefix)); }

	value_ {|inparams| params = inparams.copy; this.refresh; }

	valueAction_ {|inparams| this.value_(inparams); this.doAction }

	value { ^params.copy }

	set { |...pairs|
		pairs.pairsDo({|key val|
			params.perform(key.asSymbol.asSetter, val);
		});
		this.refresh;
		this.doAction;
	}

	sync {
		[ 'loShelfFreq', 'loShelfGain', 'loShelfRs', 'loPeakFreq', 'loPeakGain', 'loPeakRq', 'midPeakFreq', 'midPeakGain', 'midPeakRq', 'hiPeakFreq', 'hiPeakGain', 'hiPeakRq', 'hiShelfFreq', 'hiShelfGain', 'hiShelfRs' ].do({|key|
			key = (prefix ++ key).asSymbol;
			if(target.isKindOf(NodeProxy).not, {
				target.get(key, {|value| params.perform(key.asSetter, value); {this.refresh}.defer; [key, value].postln; });
			}, {
				params.perform(key.asSetter, target.get(key)); {this.refresh}.defer;
			});
		});
	}

	init {|intarget, inparams, inprefix, insr|
		var selected = -1;
		var dragY;
		params = inparams ?? { EQuiParams() };
		target = intarget;
		prefix = inprefix;
		sampleRate = insr ?? {Server.default.options.sampleRate} ? 44100;

		this.drawFunc = { |vw|
			var freqs, svals, values, bounds, zeroline;
			var min = 20, max = 22050, range = 24;
			var vlines = [100,1000,10000];
			var dimvlines = [25,50,75, 250,500,750, 2500,5000,7500];
			var hlines = [-18,-12,-6,6,12,18];
			var pt, strOffset = 11;

			bounds = vw.bounds.moveTo(0,0);

			freqs = ({|i| i } ! (bounds.width+1));
			freqs = freqs.linexp(0, bounds.width, min, max );

			values = [
				this.magResponse( freqs, sampleRate, this.coeffsBLowShelf(sampleRate, params.loShelfFreq, params.loShelfRs, params.loShelfGain)),
				this.magResponse( freqs, sampleRate, this.coeffsBPeakEQ(sampleRate, params.loPeakFreq, params.loPeakRq, params.loPeakGain)),
				this.magResponse( freqs, sampleRate, this.coeffsBPeakEQ(sampleRate, params.midPeakFreq, params.midPeakRq, params.midPeakGain)),
				this.magResponse( freqs, sampleRate, this.coeffsBPeakEQ(sampleRate, params.hiPeakFreq, params.hiPeakRq, params.hiPeakGain)),
				this.magResponse( freqs, sampleRate, this.coeffsBHiShelf(sampleRate, params.hiShelfFreq, params.hiShelfRs, params.hiShelfGain))
			].ampdb.max(-200).min(200);

			zeroline = 0.linlin(range.neg,range, bounds.height, 0, \none);

			svals = values.sum.linlin(range.neg,range, bounds.height, 0, \none);
			values = values.linlin(range.neg,range, bounds.height, 0, \none);

			vlines = vlines.explin( min, max, 0, bounds.width );
			dimvlines = dimvlines.explin( min, max, 0, bounds.width );

			pt = 5.collect({ |ind|
				(params.freqByIndex(ind).explin( min, max, 0, bounds.width ))
				@
				(params.gainByIndex(ind).linlin(range.neg,range,bounds.height,0,\none));
			});

			Pen.color_( Color.white.alpha_(0.25) );
			Pen.addRoundedRect( bounds, 6, 6 ).fill;

			Pen.color = Color.gray(0.2).alpha_(0.5);

			Pen.addRoundedRect( bounds.insetBy(0,0), 6, 6).clip;

			Pen.color = Color.gray(0.2).alpha_(0.125);

			hlines.do({ |hline,i|
				hline = hline.linlin( range.neg,range, bounds.height, 0, \none );
				Pen.line( 0@hline, bounds.width@hline )
			});
			dimvlines.do({ |vline,i|
				Pen.line( vline@0, vline@bounds.height );
			});
			Pen.stroke;

			Pen.color = Color.gray(0.2).alpha_(0.5);
			vlines.do({ |vline,i|
				Pen.line( vline@0, vline@bounds.height );
			});
			Pen.line( 0@zeroline, bounds.width@zeroline ).stroke;

			Pen.font = Font( Font.defaultSansFace, 10 );

			Pen.color = Color.gray(0.2).alpha_(0.5);
			hlines.do({ |hline|
				Pen.stringAtPoint( hline.asString ++ "dB",
					3@(hline.linlin( range.neg,range, bounds.height, 0, \none )
						- strOffset) );
			});
			vlines.do({ |vline,i|
				Pen.stringAtPoint( ["100Hz", "1KHz", "10KHz"][i],
					(vline+2)@(bounds.height - (strOffset + 1)) );
			});

			if( selected != -1, {
				var string, strBounds;

				string = [ "low shelf: %hz, %dB, rs=%",
					"peak 1: %hz, %dB, rq=%",
					"peak 2: %hz, %dB, rq=%",
					"peak 3: %hz, %dB, rq=%",
					"hi shelf: %hz, %dB, rs=%"
				][ selected ].format(
					params.freqByIndex(selected),
					params.gainByIndex(selected),
					params.bwByIndex(selected)
				);
				strBounds = string.bounds(Pen.font);
				strBounds.top = 1;
				strBounds.left = this.bounds.width - 1 - strBounds.width;
				Pen.color = Color.white.alpha_(0.8);
				Pen.addRect(strBounds);
				Pen.fill;
				Pen.stringInRect(string, strBounds, Pen.font, Color.gray(0.2).alpha_(0.5), \right);
			});

			values.do({ |svals,i|
				var color;
				color = Color.hsv(
					i.linlin(0,values.size,0,1),
					0.75, 0.5).alpha_(if( selected == i ) { 0.75 } { 0.25 });
				Pen.color = color;
				Pen.moveTo( 0@(svals[0]) );
				svals[1..].do({ |val, i|
					Pen.lineTo( (i+1)@val );
				});
				Pen.lineTo( bounds.width@(bounds.height/2) );
				Pen.lineTo( 0@(bounds.height/2) );
				Pen.lineTo( 0@(svals[0]) );
				Pen.fill;

				Pen.addArc( pt[i], 5, 0, 2pi );

				Pen.color = color.alpha_(0.75);
				Pen.stroke;

			});

			Pen.color = Color.blue(0.5);
			Pen.moveTo( 0@(svals[0]) );
			svals[1..].do({ |val, i|
				Pen.lineTo( (i+1)@val );
			});
			Pen.stroke;

		};
		this.refresh;

		this.mouseDownAction = { |vw,x,y,mod|
			var bounds;
			var pt;
			var min = 20, max = 22050, range = 24;

			bounds = vw.bounds.moveTo(0,0);
			pt = (x@y);

			selected =  (0..4).detect({ |index|
				(( params.freqByIndex(index).explin( min, max, 0, bounds.width ) )@
					( params.gainByIndex(index).linlin( range.neg, range, bounds.height, 0, \none ) ))
				.dist( pt ) <= 5;
			}) ? -1;

			vw.refresh;
		};

		this.mouseMoveAction = { |vw,x,y,mod|
			var bounds;
			var pt;
			var min = 20, max = 22050, range = 24;

			bounds = vw.bounds.moveTo(0,0);
			pt = (x@y);

			if( selected != -1 )
			{
				if(mod.isAlt,
				{
					if(dragY.isNil, {
						dragY = y;
					});
					params.setBwByIndex(selected,
						(y - dragY + params.bwByIndex(selected)
							.explin( 0.1, 10, bounds.height, 0, \none ))
						.linexp( bounds.height, 0, 0.1, 10, \none )
						.clip(if( [0,4].includes(selected) ) { 0.6 } {0.1},
						10).round(0.01));

				},
				{
					params.setFreqByIndex(selected, pt.x.linexp(0, bounds.width, min, max ).clip(20,20000).round(1));
					params.setGainByIndex(selected, pt.y.linlin( 0, bounds.height, range, range.neg, \none ).clip2( range ).round(0.25));
					dragY = nil;
				});

				this.doAction;
				vw.refresh;

			};


		};

		this.mouseUpAction = { dragY = nil };
	}

	doAction {
		action.value(this, params, prefix);
		target.set(*params.asArgsArray(prefix));
	}

	// coeffs and magResponses
	// a bit messy, but avoids dependancy/conflicts with wslib

	coeffsBPeakEQ  { arg sr = 44100, freq = 1200.0, rq = 1.0, db = 0.0;
		var a, w0, alpha, a0, a2, b1, b2, b0rz;
		a = pow(10, db/40);
		w0 = (pi * 2 * freq) / sr;
		alpha = w0.sin * 0.5 * rq;
		b0rz = (1 + (alpha / a)).reciprocal;
		a0 = (1 + (alpha * a)) * b0rz;
		a2 = (1 - (alpha * a)) * b0rz;
		b1 = 2.0 * w0.cos * b0rz;
		b2 = (1 - (alpha / a)) * b0rz.neg;
		^[[a0, b1.neg, a2], [b1, b2].neg];
	}

	coeffsBLowShelf  { arg sr = 44100, freq = 120.0, rs = 1.0, db = 0.0;
		var a, w0, sin_w0, cos_w0, alpha, i, j, k, a0, a1, a2, b0rz, b1, b2;
		a = pow(10, db/40);
		w0 = (pi * 2 * freq) / sr;
		cos_w0 = w0.cos;
		sin_w0 = w0.sin;
		alpha = sin_w0 * 0.5 * sqrt((a + a.reciprocal) * (rs - 1) + 2.0);
		i = (a+1) * cos_w0;
		j = (a-1) * cos_w0;
		k = 2 * sqrt(a) * alpha;
		b0rz = ((a+1) + j + k).reciprocal;
		a0 = a * ((a+1) - j + k) * b0rz;
		a1 = 2 * a * ((a-1) - i) * b0rz;
		a2 = a * ((a+1) - j - k) * b0rz;
		b1 = 2.0 * ((a-1) + i) * b0rz;
		b2 = ((a+1) + j - k) * b0rz.neg;
		^[[a0, a1, a2], [b1, b2].neg];
	}

	coeffsBHiShelf  { arg sr = 44100, freq = 120.0, rs = 1.0, db = 0.0;
		var a, w0, sin_w0, cos_w0, alpha, i, j, k, a0, a1, a2, b0rz, b1, b2;
		a = pow(10, db/40);
		w0 = (pi * 2 * freq) / sr;
		cos_w0 = w0.cos;
		sin_w0 = w0.sin;
		alpha = sin_w0 * 0.5 * sqrt((a + a.reciprocal) * (rs - 1) + 2.0);
		i = (a+1) * cos_w0;
		j = (a-1) * cos_w0;
		k = 2 * sqrt(a) * alpha;
		b0rz = ((a+1) - j + k).reciprocal;
		a0 = a * ((a+1) + j + k) * b0rz;
		a1 = -2.0 * a * ((a-1) + i) * b0rz;
		a2 = a * ((a+1) + j - k) * b0rz;
		b1 = -2.0 * ((a-1) - i) * b0rz;
		b2 = ((a+1) - j - k) * b0rz.neg;
		^[[a0, a1, a2], [b1, b2].neg];
	}

	magResponse { arg freqs = 1000, sr = 44100, coeffs;
		var ma, ar, size;

		#ma, ar = coeffs;
		size = ma.size.max( ar.size + 1 );

		if( freqs.isNumber ) // autoscale 20-22000
			{ freqs = (..freqs).linexp(0,freqs-1, 20, 22000); };

		case { size < 4 }
			{ ^this.magResponse2( freqs, sr, ma, ar ) }
			{ size < 7 }
			{ ^this.magResponse5( freqs, sr, ma, ar ) }
			{ ^this.magResponseN( freqs, sr, ma, ar, size ) };
	}


	magResponse2 { arg freqs, sr, ma, ar;
		var pfreq, cos1, cos2, nom, denom;
		var a0, a1, a2, b1, b2;
		var ax0, ax1, ax2, bx0, bx1;
		var radPerSmp; //= 2pi / sr;

		sr = sr ?? { Server.default.sampleRate };
		radPerSmp = 2pi / sr;

		#a0, a1, a2  = ma ++ #[ 0.0, 0.0, 0.0 ];
		#b1, b2    = ar ++ #[ 0.0, 0.0 ];

		ax0 = (a0*a0) + (a1*a1) + (a2*a2);
		ax1 = (a0*a1) + (a1*a2);
		ax2 = a0*a2;

		bx0 = 1.0 + (b1*b1) + (b2*b2);
		bx1 = b1 + (b1*b2);

		^freqs.collect({ arg freq;
			pfreq = freq * radPerSmp;
			cos1	= cos( pfreq );
			cos2	= cos( pfreq * 2 );

			nom = ax0 + (2 * ( ( ax1 * cos1) + ( ax2 * cos2) ));

			denom = bx0 + (2 * ( ( bx1 * cos1) + ( b2 * cos2) ));

			sqrt( nom / denom );
		});
	}


	magResponse5 { arg freqs, sr, ma, ar;
		var pfreq, cos1, cos2, cos3, cos4, cos5, nom, denom;
		var a0, a1, a2, a3, a4, a5, b1, b2, b3, b4, b5;
		var ax0, ax1, ax2, ax3, ax4, ax5, bx0, bx1, bx2, bx3, bx4;
		var radPerSmp; //= 2pi / sr;

		sr = sr ?? { Server.default.sampleRate };
		radPerSmp = 2pi / sr;

		#a0, a1, a2, a3, a4, a5 = ma ++ #[ 0.0, 0.0, 0.0, 0.0, 0.0, 0.0 ];
		#b1, b2, b3, b4, b5     = ar ++ #[ 0.0, 0.0, 0.0, 0.0, 0.0 ];

		ax0 = (a0*a0) + (a1*a1) + (a2*a2) + (a3*a3) + (a4*a4) + (a5*a5);
		ax1 = (a0*a1) + (a1*a2) + (a2*a3) + (a3*a4) + (a4*a5);
		ax2 = (a0*a2) + (a1*a3) + (a2*a4) + (a3*a5);
		ax3 = (a0*a3) + (a1*a4) + (a2*a5);
		ax4 = (a0*a4) + (a1*a5);
		ax5 = a0*a5;

		bx0 = 1.0 + (b1*b1) + (b2*b2) + (b3*b3) + (b4*b4) + (b5*b5);
		bx1 = b1 + (b1*b2) + (b2*b3) + (b3*b4) + (b4*b5);
		bx2 = b2 + (b1*b3) + (b2*b4) + (b3*b5);
		bx3 = b3 + (b1*b4) + (b2*b5);
	 	bx4 = b4 + (b1*b5);

		^freqs.collect({ arg freq;
			var complex;
			pfreq = freq * radPerSmp;
			cos1	= cos( pfreq );
			cos2	= cos( pfreq * 2 );
			cos3	= cos( pfreq * 3 );
			cos4	= cos( pfreq * 4 );
			cos5	= cos( pfreq * 5 );

			nom = ax0 + (2 * ( ( ax1 * cos1) + ( ax2 * cos2) + ( ax3 * cos3) +
				( ax4 * cos4) + ( ax5 * cos5) ));

			denom = bx0 + (2 * ( ( bx1 * cos1) + ( bx2 * cos2) + ( bx3 * cos3) +
		         ( bx4 * cos4) + ( b5 * cos5) ));

			sqrt( nom / denom );
		});
	}

	magResponseN { arg freqs, sr, ma, ar, size; // way slower, but can handle higher order
		var radPerSmp; //= 2pi / sr;
		var ax, bx;
		var nom, denom;
		var cosn,  pfreq;

		sr = sr ?? { Server.default.sampleRate };
		radPerSmp = 2pi / sr;

		size = size ?? { ma.size.max( ar.size + 1 ) };

		ma = ma.extend( size, 0.0 );
		ar = ([1] ++ ar).extend( size, 0.0 );

		ax = (size+1).collect({ |i| (ma[i..] *.s ma).sum });
		bx = (size+1).collect({ |i| (ar[i..] *.s ar).sum });

		^freqs.collect({ arg freq;
			pfreq = freq * radPerSmp;
			cosn = [0.5] ++ size.collect({ |i| cos( pfreq * (i+1) ) });

			nom = (cosn * ax).sum * 2;
			denom = (cosn * bx).sum * 2;

			sqrt( nom / denom );
		});
	}
}

EQuiParams {
	var <>loShelfFreq, <>loShelfGain, <>loShelfRs;
	var <>loPeakFreq, <>loPeakGain, <>loPeakRq;
	var <>midPeakFreq, <>midPeakGain, <>midPeakRq;
	var <>hiPeakFreq, <>hiPeakGain, <>hiPeakRq;
	var <>hiShelfFreq, <>hiShelfGain, <>hiShelfRs;
	classvar bands = #[\loShelf, \loPeak, \midPeak, \hiPeak, \hiShelf];

	*new {|loShelfFreq = 100, loShelfGain = 0, loShelfRs = 1, loPeakFreq = 250, loPeakGain = 0, loPeakRq = 1, midPeakFreq = 1000, midPeakGain = 0, midPeakRq = 1, hiPeakFreq = 3500, hiPeakGain = 0, hiPeakRq = 1,hiShelfFreq = 6000, hiShelfGain = 0, hiShelfRs = 1|
		^super.newCopyArgs(loShelfFreq, loShelfGain, loShelfRs, loPeakFreq, loPeakGain, loPeakRq, midPeakFreq, midPeakGain, midPeakRq, hiPeakFreq, hiPeakGain, hiPeakRq, hiShelfFreq, hiShelfGain, hiShelfRs);
	}

	asArgsArray {|prefix|
		^[ 'loShelfFreq', loShelfFreq, 'loShelfGain', loShelfGain, 'loShelfRs', loShelfRs, 'loPeakFreq', loPeakFreq, 'loPeakGain', loPeakGain, 'loPeakRq', loPeakRq, 'midPeakFreq', midPeakFreq, 'midPeakGain', midPeakGain, 'midPeakRq', midPeakRq, 'hiPeakFreq', hiPeakFreq, 'hiPeakGain', hiPeakGain, 'hiPeakRq', hiPeakRq, 'hiShelfFreq', hiShelfFreq, 'hiShelfGain', hiShelfGain, 'hiShelfRs', hiShelfRs ]
	}

	freqByIndex{|index| ^this.perform((bands[index] ++ 'Freq').asSymbol) }
	gainByIndex{|index| ^this.perform((bands[index] ++ 'Gain').asSymbol) }
	bwByIndex{|index|
		var suffix;
		suffix = if(index.inclusivelyBetween(1, 3), {"Rq"}, {"Rs"});
		^this.perform((bands[index] ++ suffix).asSymbol)
	}

	setFreqByIndex{|index, val| this.perform((bands[index] ++ 'Freq_').asSymbol, val) }
	setGainByIndex{|index, val| this.perform((bands[index] ++ 'Gain_').asSymbol, val) }
	setBwByIndex{|index, val|
		var suffix;
		suffix = if(index.inclusivelyBetween(1, 3), {"Rq_"}, {"Rs_"});
		this.perform((bands[index] ++ suffix).asSymbol, val)
	}

	storeOn { arg stream;
		stream << this.class.name << "(" <<*
			[loShelfFreq, loShelfGain, loShelfRs, loPeakFreq, loPeakGain, loPeakRq, midPeakFreq, midPeakGain, midPeakRq, hiPeakFreq, hiPeakGain, hiPeakRq, hiShelfFreq, hiShelfGain, hiShelfRs] <<")"
	}

}

+ UGen {

	equi {|params, prefix = "", lag = 0.1|
		var chain, lagCtl;
		params = params ?? {EQuiParams()}; // defaults
		chain = this;
		lagCtl = NamedControl.kr(prefix ++ "lagEQ", lag);

		chain = BLowShelf.ar( chain,
			NamedControl.kr(prefix ++ "loShelfFreq", params.loShelfFreq, lagCtl),
			NamedControl.kr(prefix ++ "loShelfRs", params.loShelfRs, lagCtl),
			NamedControl.kr(prefix ++ "loShelfGain", params.loShelfGain).varlag(lagCtl, warp:\lin)
		);
		chain = BPeakEQ.ar( chain,
			NamedControl.kr(prefix ++ "loPeakFreq", params.loPeakFreq, lagCtl),
			NamedControl.kr(prefix ++ "loPeakRq", params.loPeakRq, lagCtl),
			NamedControl.kr(prefix ++ "loPeakGain", params.loPeakGain).varlag(lagCtl, warp:\lin)
		);
		chain = BPeakEQ.ar( chain,
			NamedControl.kr(prefix ++ "midPeakFreq", params.midPeakFreq, lagCtl),
			NamedControl.kr(prefix ++ "midPeakRq", params.midPeakRq, lagCtl),
			NamedControl.kr(prefix ++ "midPeakGain", params.midPeakGain).varlag(lagCtl, warp:\lin)
		);
		chain = BPeakEQ.ar( chain,
			NamedControl.kr(prefix ++ "hiPeakFreq", params.hiPeakFreq, lagCtl),
			NamedControl.kr(prefix ++ "hiPeakRq", params.hiPeakRq, lagCtl),
			NamedControl.kr(prefix ++ "hiPeakGain", params.hiPeakGain).varlag(lagCtl, warp:\lin)
		);
		chain = BHiShelf.ar( chain,
			NamedControl.kr(prefix ++ "hiShelfFreq", params.hiShelfFreq, lagCtl),
			NamedControl.kr(prefix ++ "hiShelfRs", params.hiShelfRs, lagCtl),
			NamedControl.kr(prefix ++ "hiShelfGain", params.hiShelfGain, lagCtl)
		);
		chain = this.removeBadValues( chain );

		^chain;
	}

	removeBadValues {|chain|
		var good;
		good = BinaryOpUGen('==', CheckBadValues.kr(chain, 0, 0), 0);
		^(chain * good);
	}
}

+ SequenceableCollection {

	equi { |params, prefix = "", lag = 0.1| ^this.multiChannelPerform('equi', params, prefix, lag) }

}

+ Ndef {

	equi { |params, lag = 0.1, index = 1000|
		var window;
		this[index] = \filter -> {|in| in.equi(params, nil, lag) };
		window = Window.new("Ndef('" ++ key ++ "') EQui", EQui.sizeHint).front;
		EQui(window, target:this);
	}
}
		