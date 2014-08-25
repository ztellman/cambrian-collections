package clojure.lang;

import java.util.Iterator;

public class PersistentVector2 extends APersistentVector implements IObj,
	IEditableCollection, IReduce {
    final Object e0;
    final Object e1;
    private final IPersistentMap meta;
    private int hash = -1;
    private int hasheq = -1;

    PersistentVector2(IPersistentMap meta, Object e0, Object e1) {
	this.meta = meta;
	this.e0 = e0;
	this.e1 = e1;
    }

    public PersistentVector2(Object e0, Object e1) {
	this.meta = null;
	this.e0 = e0;
	this.e1 = e1;
    }

    public IPersistentMap meta() {
	return meta;
    }

    public IObj withMeta(IPersistentMap meta) {
	return new PersistentVector2(meta, e0, e1);
    }

    public Object nth(int i) {
	switch (i) {
	case 0:
	    return e0;
	case 1:
	    return e1;
	default:
	    throw new IndexOutOfBoundsException();
	}
    }

    public Object nth(int i, Object notFound) {
	switch (i) {
	case 0:
	    return e0;
	case 1:
	    return e1;
	default:
	    return notFound;
	}
    }

    public int count() {
	return 2;
    }

    public IPersistentVector empty() {
	return PersistentVector0.EMPTY;
    }

    public IPersistentVector assocN(int i, Object val) {
	switch (i) {
	case 0:
	    return new PersistentVector2(meta, val, e1);
	case 1:
	    return new PersistentVector2(meta, e0, val);
	case 2:
	    return cons(val);
	default:
	    throw new IndexOutOfBoundsException();
	}
    }

    public IPersistentVector cons(Object val) {
	return new PersistentVector3(meta, e0, e1, val);
    }

    public ITransientCollection asTransient() {
	ITransientCollection coll = PersistentVector.EMPTY.asTransient();
	return (ITransientVector) coll.conj(e0).conj(e1);
    }

    public IPersistentVector pop() {
	return new PersistentVector1(meta, e0);
    }

    public Object kvreduce(IFn f, Object init) {
	init = f.invoke(init, 0, e0);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, 1, e1);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public Object reduce(IFn f) {
	Object init = e0;
	init = f.invoke(init, e1);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public Object reduce(IFn f, Object init) {
	init = f.invoke(init, e0);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	init = f.invoke(init, e1);
	if (RT.isReduced(init)) {
	    return ((IDeref) init).deref();
	}
	return init;
    }

    public int hashCode() {
	if (this.hash == -1) {
	    int hash = 1;
	    hash = (31 * hash) + (e0 == null ? 0 : e0.hashCode());
	    hash = (31 * hash) + (e1 == null ? 0 : e1.hashCode());
	    this.hash = hash;
	}
	return hash;
    }

    public int hasheq() {
	if (this.hasheq == -1) {
	    int hash = 1;
	    hash = (31 * hash) + Util.hasheq(e0);
	    hash = (31 * hash) + Util.hasheq(e1);
	    hash = Murmur3.mixCollHash(hash, 2);
	    this.hasheq = hash;
	}
	return hasheq;
    }

    public boolean equals(Object o) {
	if (o instanceof PersistentVector2) {
	    return Util.equals(e0, ((PersistentVector2) o).e0)
		    && Util.equals(e1, ((PersistentVector2) o).e1);
	} else {
	    return super.equals(o);
	}
    }

    public boolean equiv(Object o) {
	if (o instanceof PersistentVector2) {
	    return Util.equiv(e0, ((PersistentVector2) o).e0)
		    && Util.equiv(e1, ((PersistentVector2) o).e1);
	} else {
	    return super.equiv(o);
	}
    }

    public Iterator iterator() {
	return new Iterator() {
	    int i = 0;

	    public boolean hasNext() {
		return i < 2;
	    }

	    public Object next() {
		return nth(i++);
	    }

	    public void remove() {
		throw new UnsupportedOperationException();
	    }
	};
    }

    public ISeq seq() {
	return IteratorSeq.create(iterator());
    }
}